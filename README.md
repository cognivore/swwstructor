# swwstructor

A **single-tenant website constructor** built on the
[`stickywebwm`](../stickywebwm) layout engine. Owners author content as data
(sections) and a theme, enable plugins (Stripe checkout, Shopify catalogue), and
the engine places everything *algorithmically* — the owner makes **zero layout
decisions**. One NixOS box runs **several** of these single-tenant sites.

It is the *content + template + theme + multipage + plugin* layer described in
the engine's `CONSTRUCTOR.md`. It depends on the engine; it never reimplements
layout, measurement, or stickiness, and it keeps the engine's type discipline:
final tagless throughout, strong newtypes, and the engine's proofs stay green.

---

## What it does

- **Content is data.** A site is a single `site.json`/`site.yaml`: a theme, a
  nav, and pages of typed *sections* (`masthead`, `navStrip`, `ribbon`, `hero`,
  `featureSplit`, `storyRow`, `productGrid`, `richColumns`, `gallery`, `ctaBand`,
  `contact`, `prose`, `footer`). Adding a product or a section is a content edit,
  never a code edit.
- **The engine lays it out.** Each section becomes a `(layout term, content map)`
  pair; the engine solves geometry, re-homes columns to one on a phone, and
  drives stickiness (a pinned figure rides along inside its story). No media
  queries, no CSS authored by the owner.
- **Themes are values.** Palette, fonts, density and geometry live in a `Theme`;
  its single CSS interpreter is the only place branding exists. The same engine
  renders a black-on-white newspaper and a pink confectionery shop from data.
- **Stripe via the admin WebUI.** The owner signs in at `/admin` and pastes their
  Stripe keys (publishable / secret / webhook). Keys are **encrypted at rest**
  with AES-256-GCM under a per-instance master key — never in logs, the Nix
  store, git, or the JS bundle. Buy buttons drive Stripe-hosted Checkout; the
  amount is always resolved server-side from the owner's content.
- **The New York Times benchmark.** `test/Spec.hs` builds an NYT-style front page
  *entirely from content data* and proves it is well-formed at desktop and phone,
  re-homes to one column, conserves windows, pins its lead figure, and matches
  the engine's `nytFront` archetype. 55/55 green.

---

## Layout

```
src/Swwstructor/            the PURE constructor core (base+containers+text only)
  Html.hs                   safe HTML fragments
  Money.hs                  Cents / Currency / Price
  Block.hs                  the content DSL (BlockSym) + HTML/Plain interpreters + JSON codec
  Theme.hs                  Theme value + CSS interpreter + JSON codec
  Content.hs                section/page/site schema + JSON codec ("content is data")
  Templates.hs              SectionSpec -> Section (layout term x block map) — reproduces NYT
  Site.hs                   the SSR pipeline + two-pass client + sticky automaton
  Checkout.hs               Stripe form-encoding (pure) + MockCheckoutT
  Plugins/Shopify.hs        Storefront products -> productGrid (section provider)
  Plugins/Stripe.hs         site -> buy registry; checkout section
  Adapter/UAL.hs            universal-art-link content -> SiteSpec (content-source adapter)
app/                        the server (scotty): SSR host + admin WebUI + secret store
  Main.hs                   boot from the environment
  Swwstructor/Server/       SecretStore (AES-GCM), Stripe (live), Loader (JSON+YAML), Admin, App
test/Spec.hs                acceptance tests incl. the NYT benchmark (hand-rolled, offline)
ts/                         the admin live-preview client (consumes the stickywebwm TS SDK)
automation/                 rust-script automation: provision.rs, ship.rs, site.rs (NOT shell)
nix/                        flake packaging + the multi-instance NixOS module
deploy/                     a NixOS EC2 host running three sites on one box
sites/{nyt,okashi,hello}/   example content (JSON + a YAML site)
```

---

## Download and run

Two tools needed: **`nix`** (flakes enabled) and **`rageveil`** (the secret store)
on your `PATH`. Then:

```sh
git clone git@github.com:cognivore/swwstructor.git && cd swwstructor
nix run .#run            #  → http://localhost:3000  (admin at /admin)
```

`nix run .#run` (a thin launcher; engine + everything else is pinned in the flake)
sources its secrets from **rageveil** and boots the server. Put these in rageveil
(all optional — anything absent falls back, see below):

| rageveil path | becomes | used for |
|---|---|---|
| `swwstructor/master` | `SWW_MASTER_KEY` (64 hex) | the at-rest secret-store key |
| `swwstructor/admin` | `SWW_ADMIN_PASSWORD` | admin login |
| `swwstructor/stripe/sk` | `SWW_STRIPE_SK` | Stripe secret key |
| `swwstructor/stripe/pk` | `SWW_STRIPE_PK` | Stripe publishable key |
| `swwstructor/stripe/webhook` | `SWW_STRIPE_WEBHOOK` | webhook signing secret |

With those set, it just works — including checkout (the Stripe keys are seeded
into the encrypted store at boot, never echoed, never written in plaintext). With
**none** set it still runs: an ephemeral master key + a generated admin password
(logged once), and you enter Stripe keys via `/admin`. rageveil values are never
printed — only which variables were found.

Pick a different site or port with env: `SWW_SITE_DIR=sites/okashi PORT=8080 nix
run .#run` (bundled sites: `nyt`, `okashi`, `hello`). `just run` is the same thing.

## Dev

```sh
nix develop                       # ghc, cabal, node/tsc, rust-script, age, just, aws
just test                         # acceptance tests incl. the NYT benchmark (offline)
just ts-check                     # tsc --noEmit + node --test
nix build .#swwstructor-server    # the server package (runs the test-suite inside)
```

---

## Stripe (admin WebUI + secrets)

The owner never edits a config file for keys. They sign in at `/admin` and paste:

- **Publishable key** (`pk_…`, browser-safe),
- **Secret key** (`sk_…`, backend only — drives `POST /v1/checkout/sessions`),
- **Webhook signing secret** (`whsec_…`, verifies `POST /stripe/webhook`).

Submitting a blank field leaves that key unchanged (rotate one at a time). Keys
are encrypted at rest in `<data dir>/secrets.enc`. Use **test** keys for demos
(`4242 4242 4242 4242`). The buy flow: a buy button `POST`s to `/buy/:id`, the
server resolves the priced line item from the owner's content, creates a hosted
Checkout session, and `303`-redirects to `checkout.stripe.com`. The webhook fails
**closed**: it returns non-2xx unless a webhook secret is configured *and* the
`Stripe-Signature` HMAC verifies. Request bodies are size-capped before parsing,
and the admin session token is master-key-HMAC'd, constant-time-compared, and
revoked on logout.

The per-instance **master key** (the one operator secret) is supplied at boot via
`SWW_MASTER_KEY` (age-decrypted by the NixOS unit). Everything else is
owner-entered.

---

## Deploy (boot an AWS box; several sites on one machine)

Automation is **rust-script**, not shell (`automation/*.rs`, driven by `just`):

```sh
just site-new times nyt.example.com 4001     # scaffold a site + age secrets + a Nix snippet
just provision                               # idempotently boot an EC2 NixOS box, write state
# point DNS A-records for each domain at the box's IP
just ship-boot                               # build the closure elsewhere, copy it, activate
just ship                                    # subsequent updates (switch)
```

`deploy/flake.nix` is a NixOS EC2 host that runs **three** instances (nyt,
okashi, hello) via `services.swwstructor.instances` — each its own systemd unit,
port, content dir and age-decrypted secrets, fronted by Caddy + ACME. The box
never compiles (`nix.settings.max-jobs = 0`); closures are built on a builder and
`nix copy`'d over. See `automation/README.md` for the full runbook.

---

## Verification matrix

| Check | Command | Expect |
|---|---|---|
| Constructor tests (incl. NYT benchmark) | `just test` | `55/55 PASS` |
| Server builds (tests run inside) | `nix build .#swwstructor-server` | builds green |
| SSR | run server, `curl /health` then `curl /` | `ok`; positioned boxes |
| Responsive re-home | `curl '/layout?w=390&path=/'` vs `?w=1280` | all `left:0px` vs multiple columns |
| TS types + unit | `just ts-check` | 0 errors; tests pass |
| Secret hygiene | grep store/bundle/logs for `sk_`, admin tokens | no matches |
| Engine proofs unchanged | (in ../stickywebwm) `cabal test`; `StickyProof.hs` | `94/94`; `11/11` |

---

## Invariants (inherited from the engine, enforced here)

1. **Final tagless, strong newtypes.** New capabilities are typeclasses; new
   behaviours are interpreters; domain concepts are newtypes, never bare
   `String`/`Int`/`Text`.
2. **One source of layout truth = the Haskell engine.** TypeScript adds content,
   rendering glue, and a preview — never a second solver.
3. **Sticky only within a container; sticky overlay reserves its space.**
4. **Conservation + determinism**: the placement id set is identical at every
   viewport.
5. **Secrets never touch the layout pipeline, the Nix store (plaintext), the JS
   bundle, logs, or git.** Test-mode keys for demos.
