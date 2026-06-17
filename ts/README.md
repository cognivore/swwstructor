# swwstructor (TypeScript client)

The admin/preview web client for **swwstructor**, a single-tenant website
constructor built on the [`stickywebwm`](../../stickywebwm/ts) layout engine.

It provides two things:

1. **Type-safe content interfaces** (`src/content.ts`) — a TypeScript twin of the
   constructor's content JSON schema (authored by the Haskell
   `Swwstructor.Content` codec), so an admin editor can build a `SiteSpec` with
   full type-safety and the JSON round-trips through the Haskell decoder.
2. **A live layout preview** (`src/preview.ts`) — fetches the engine-solved
   layout `Document` for a page and renders it responsively with sticky-overlay
   behaviour, consuming the `stickywebwm` TS SDK.

> The layout engine is the single source of truth. This client adds content and
> rendering glue and a preview only — it never runs a second solver. The preview
> only ever fetches a layout `Document`; **no key or secret ever reaches the
> bundle.**

## Install

```sh
cd ts
npm install        # resolves stickywebwm via file:../../stickywebwm/ts
```

The `stickywebwm` dependency is linked from `../stickywebwm/ts` (its prebuilt
`dist/` is what gets resolved).

## Content model

```ts
import { emptySite, isSiteSpec, type SiteSpec, type SectionSpec } from "swwstructor";

const site: SiteSpec = emptySite();
site.title = "Okashi";
site.pages[0].sections.push({
  section: "hero",
  headline: "Seasonal wagashi",
  cta: { label: "Shop", href: "/shop" },
});

// Narrow JSON loaded from disk / the wire before use:
const data: unknown = JSON.parse(raw);
if (isSiteSpec(data)) {
  // data is SiteSpec here
}
```

`SectionSpec` is a discriminated union on the `section` tag (`"masthead"`,
`"navStrip"`, `"ribbon"`, `"hero"`, `"featureSplit"`, `"storyRow"`,
`"productGrid"`, `"richColumns"`, `"gallery"`, `"ctaBand"`, `"contact"`,
`"prose"`, `"footer"`), so editor code gets exhaustive type narrowing.

## Embedding the preview (admin page, browser)

The constructor server exposes `GET /admin/preview.json?path=<pagePath>`, which
returns the `stickywebwm` wire `Document` for that page's layout. In the browser,
drive it with the GHC-JS solver bundle:

```ts
import { loadBundle, mountPreview } from "swwstructor";

// 1. Load the GHC-JS solver bundle (the Nix `solver-js` output) — it attaches
//    the solveJSON/wfJSON/... string functions to a global.
await import("/static/stickywebwm-solver.js");

// 2. Wrap it as a Solver (BundleSolver).
const solver = loadBundle(globalThis);

// 3. Mount a live, responsive, sticky preview of a page.
const app = await mountPreview(document.getElementById("preview")!, {
  path: "/",       // page path to preview
  solver,          // the BundleSolver
  // baseUrl: "",  // defaults to same origin
});

// After an edit changes the page's layout:
await app.refresh();

// Tear down:
app.destroy();
```

`mountPreview` fetches the page's `Document`, then calls the SDK's `mount` with a
`render` callback that labels each placed box with its window id + type, so the
preview is readable without real content. Width changes are re-solved by the SDK
runtime automatically; `refresh()` re-fetches and re-mounts (use it after an
edit). `loadBundle`, `BundleSolver`, and `NativeSolver` are re-exported from
`swwstructor`, so the admin page can import everything from one place.

### Node / SSR / tests

Use `NativeSolver` (spawns the native `stickywebwm-solve` CLI) instead of the
bundle, and the DOM-free `solvePreview(doc, solver, width, viewport)` helper to
get placements without mounting:

```ts
import { NativeSolver, solvePreview } from "swwstructor";

const solver = new NativeSolver("/path/to/stickywebwm-solve");
const placements = await solvePreview(doc, solver, 1180, { w: 1180, h: 820 });
solver.close();
```

## Develop

```sh
npm run typecheck   # tsc --noEmit  -> 0 errors
npm run build       # tsc           -> dist/
npm test            # node --test
```

The test suite (`test/preview.test.ts`) always checks the content helpers. The
solver-dependent preview assertions (two columns wide, re-home narrow, id-set
conservation) run only when `SWW_SOLVE` points at a `stickywebwm-solve` binary;
without it they skip cleanly:

```sh
SWW_SOLVE=/path/to/stickywebwm-solve npm test
```
