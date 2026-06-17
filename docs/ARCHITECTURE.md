# swwstructor architecture

swwstructor is the *content + template + theme + multipage + plugin* layer on top
of the `stickywebwm` engine. The engine owns layout (placement, sizing,
stickiness) and its proofs; swwstructor owns everything an owner authors. One
description, many interpretations — final tagless, end to end.

## The data flow (one page)

```
site.json / site.yaml                      owner-authored content (data)
        │  Loader (JSON | HsYAML→JSON)
        ▼
SiteSpec  =  Theme × nav × [PageSpec]       Swwstructor.Content   (+ JSON codec)
        │  pageSection : PageSpec → Section
        ▼
Section   =  Document × Map WinId Block      Swwstructor.Templates (the template library)
        │                              │
        │ build/solveWith              │ buildBlock
        ▼                              ▼
[Placement] (Geom interpreter)         HtmlBlock (content interpreter)
        └──────────────┬───────────────┘
                       ▼
        one absolutely-positioned <div> per (rect, html)   Swwstructor.Site
                       │  + Theme→CSS (Swwstructor.Theme)  + two-pass client + sticky automaton
                       ▼
                  the served page
```

The owner makes **zero layout decisions**. A section is a *template × data* pair
(the engine's "feature" pattern generalised): `FeatureSplit ρ g main rail`
becomes `split2 ρ g (stack mainStories) (stack railStories)`; a `Story` with a
sticky image becomes a `stack` whose figure is `stickyTo` the story's other
windows, so it pins only within its own story (sticky-within-container).

## The six layers (CONSTRUCTOR §3)

| # | Layer | Module(s) | What it owns |
|---|---|---|---|
| 1 | Template library | `Templates` | `SectionSpec → Section`; reproduces the NYT front |
| 2 | Content-as-data | `Content` (+ `Block`) | the section/page/site schema + JSON codec |
| 3 | Theme | `Theme` | a `Theme` value → the one stylesheet (`themeCss`) |
| 4 | Site / routing | `Site` + `Server.App` | `Map Path Page`, the generic multipage host |
| 5 | Content-source adapter | `Adapter.UAL` | universal-art-link content → `SiteSpec` |
| 6 | Plugins as section providers | `Plugins.{Shopify,Stripe}` | Shopify→`productGrid`; Stripe buy registry |

## Final tagless, twice

- **Layout** is the engine's `LayoutSym` (`leaf`/`stack`/`splitN`), interpreted by
  `Geom`/`Wf`/`Meas`/`Pretty`. swwstructor only *constructs* terms; it never adds
  a solver.
- **Content** is `BlockSym` (`heading`/`kicker`/`paragraph`/`priceTag`/`buyButton`/
  …), with initial encoding `Block`, interpreters `HtmlBlock` (SSR) and
  `PlainBlock` (text/tests), and a JSON codec so content is data.
- **Effects** are capabilities: `MonadCheckout` with `MockCheckoutT` (pure, tests)
  and the live `Server.Stripe` interpreter (`http-client`). Secrets are a value
  type (`SecretValue`, redacting `Show`) behind the `SecretStore`.

Strong newtypes throughout: `Cents`, `Currency`, `Price`, `BuyTarget`, `Url`,
`MasterKey`, `SecretValue`, `Color`. No bare `String`/`Int`/`Text` for a domain
concept.

## Responsiveness & stickiness (inherited, not reimplemented)

- **Re-home by viewport class.** A `splitN` tiles ≥2 columns at the wide
  breakpoint and rewrites to a `stack` at one column on a phone — for free. The
  server's `/layout?w=` re-solves at the device width (server-side); the client
  never ships a solver.
- **Two-pass measure.** First paint uses analytic heights; the dependency-free
  client measures real DOM heights and re-packs exactly (`POST /layout` with
  overrides). Faithful to the reference app.
- **Sticky overlay.** A pinned window becomes a viewport-fixed overlay while its
  in-flow space stays reserved; the solve never reads a mode. The client mirrors
  `StickyWM.Sticky.stickyStep` (three phases + hysteresis = no thrash).

## Stripe & secrets (the admin WebUI)

The owner enters keys at `/admin` (password-gated). Keys are encrypted at rest
with AES-256-GCM under a per-instance master key (`SWW_MASTER_KEY`, age-decrypted
by the systemd unit at boot — the single operator secret). The buy flow resolves
the priced line item server-side from the owner's content (`siteBuyRegistry`),
creates a hosted Checkout session, and `303`-redirects to Stripe. Webhooks are
HMAC-verified. Secrets never reach the layout pipeline, the Nix store (plaintext),
the JS bundle, logs, or git.

## Multi-instance deployment

`nixosModules.swwstructor` turns `services.swwstructor.instances.<name>` into one
systemd unit + one Caddy vhost each — several single-tenant sites on one NixOS
box, each with its own port, content dir, and age-decrypted secrets. Closures are
built on a builder and `nix copy`'d to the box (which never compiles). Automation
is rust-script (`automation/*.rs`), not shell.

## Where to read first

`test/Spec.hs` (the NYT benchmark + the acceptance proofs), then `Templates.hs`
(how content becomes layout), then `Site.hs` (how it renders). The wire contract
with the engine is `../stickywebwm/docs/WIRE.md`.
