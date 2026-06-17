/**
 * `node --test` suite for the swwstructor TS client.
 *
 * Two parts:
 *
 *   1. Content sanity (always runs, no solver needed): {@link emptySite} and
 *      {@link isSiteSpec} behave — a fresh site is a valid site, and the guard
 *      rejects junk.
 *
 *   2. Preview / solver responsiveness (runs only when a solver binary is
 *      available via the `SWW_SOLVE` env var, the path to `stickywebwm-solve`).
 *      We build a small {@link Document} with the SDK builder — a `split2` of two
 *      `stack`s with a `sticky(...)` figure, mirroring
 *      `stickywebwm/ts/test/solve.test.ts` — drive the real native solver through
 *      {@link solvePreview}, and assert:
 *        (a) at a wide width there are two columns (some placement has `x > 0`),
 *        (b) at a narrow width everything re-homes (`x == 0` for all),
 *        (c) the placement id set is identical at both widths (conservation).
 *      With `SWW_SOLVE` unset, those assertions are skipped cleanly so the file
 *      still passes `node --test`.
 *
 * The SDK runtime values are imported from the installed `stickywebwm` package
 * (its built `dist/`); types are imported type-only (erased at runtime). Our own
 * `content.ts` is strip-only-clean, so it is imported directly from source.
 */

import { test } from "node:test";
import assert from "node:assert/strict";

// Runtime values from the installed SDK (resolves to stickywebwm/ts/dist).
import {
  stack,
  leaf,
  split2,
  figure,
  prose,
  headline,
  chrome,
  sticky,
  NativeSolver,
} from "stickywebwm";
// Type-only imports cost nothing at runtime.
import type { Document, Placement, Viewport } from "stickywebwm";

// Our preview helper (strip-only-clean: a thin pass-through to the solver).
// Imported from the .ts source directly so `node --test` (type-stripping) can
// run this file with no build step — mirroring the SDK's own test convention.
import { solvePreview } from "../src/preview.ts";
// Our content helpers (pure functions over plain objects).
import { emptySite, isSiteSpec, type SiteSpec } from "../src/content.ts";

/* -------------------------------------------------------------------------- */
/* Content sanity (no solver)                                                 */
/* -------------------------------------------------------------------------- */

test("emptySite() is a valid, minimal SiteSpec", () => {
  const site = emptySite();
  assert.equal(typeof site.title, "string");
  assert.ok(Array.isArray(site.nav), "nav is an array");
  assert.equal(site.nav.length, 0, "empty nav");
  assert.equal(site.pages.length, 1, "one starter page");
  assert.equal(site.pages[0]?.path, "/", "starter page is at /");
  assert.deepEqual(site.pages[0]?.sections, [], "starter page has no sections");
  // The value it returns must satisfy its own guard.
  assert.ok(isSiteSpec(site), "emptySite() passes isSiteSpec");
});

test("isSiteSpec accepts a well-formed site and rejects junk", () => {
  const good: SiteSpec = {
    title: "Demo",
    nav: [{ label: "Home", href: "/" }],
    pages: [{ path: "/", title: "Home", sections: [] }],
  };
  assert.ok(isSiteSpec(good));

  // Negatives: wrong shapes must not narrow.
  assert.equal(isSiteSpec(null), false, "null is not a site");
  assert.equal(isSiteSpec(undefined), false, "undefined is not a site");
  assert.equal(isSiteSpec(42), false, "a number is not a site");
  assert.equal(isSiteSpec("site"), false, "a string is not a site");
  assert.equal(isSiteSpec({}), false, "an empty object is not a site");
  assert.equal(
    isSiteSpec({ title: "no pages" }),
    false,
    "missing pages is not a site",
  );
  assert.equal(
    isSiteSpec({ pages: [] }),
    false,
    "missing title is not a site",
  );
  assert.equal(
    isSiteSpec({ title: 1, pages: [] }),
    false,
    "non-string title is not a site",
  );
});

/* -------------------------------------------------------------------------- */
/* Preview document + solver responsiveness                                   */
/* -------------------------------------------------------------------------- */

/**
 * A small layout: a masthead/nav header above a 2-column split. The main column
 * carries a sticky hero figure (pinned while its prose is read), then prose; the
 * rail carries an aside and a figure. Mirrors the shape of the SDK's `pastry`
 * example but smaller.
 */
function demoDoc(): Document {
  return stack([
    leaf(chrome("mast", "logo", 64)),
    leaf(chrome("nav", "nav", 28)),
    split2(
      0.62,
      24,
      stack([
        leaf(sticky(0.1, ["blurb"], figure("hero", 1.6, 320))),
        leaf(headline("h", 24)),
        leaf(prose("blurb", 220)),
        leaf(prose("p1", 180)),
      ]),
      stack([
        leaf(chrome("aside", "hours", 120)),
        leaf(figure("seasonal", 1.2, 200)),
        leaf(prose("note", 120)),
      ]),
    ),
  ]);
}

const WIDE: Viewport = { w: 1180, h: 820 };
const NARROW: Viewport = { w: 380, h: 800 };

const SOLVE_BIN = process.env.SWW_SOLVE;

test("preview: two columns wide, re-home narrow, ids conserved", async (t) => {
  if (!SOLVE_BIN) {
    t.skip(
      "SWW_SOLVE not set — skipping solver-dependent preview assertions " +
        "(set SWW_SOLVE to the stickywebwm-solve binary to run them)",
    );
    return;
  }

  const doc = demoDoc();
  const solver = new NativeSolver(SOLVE_BIN);
  try {
    const wide: Placement[] = await solvePreview(doc, solver, 1180, WIDE);
    const narrow: Placement[] = await solvePreview(doc, solver, 380, NARROW);

    // (a) Wide: a real two-column layout means at least one box starts past x=0.
    assert.ok(
      wide.some((p) => p.rect.x > 0),
      "wide viewport should place some boxes at x > 0 (two columns)",
    );

    // (b) Narrow: a single column re-homes everything to x == 0.
    for (const p of narrow) {
      assert.equal(
        p.rect.x,
        0,
        `narrow viewport: ${p.id} should re-home to x = 0`,
      );
    }

    // (c) Conservation: the placement id set is identical at every viewport.
    const idsWide = wide.map((p) => p.id).sort();
    const idsNarrow = narrow.map((p) => p.id).sort();
    assert.deepEqual(
      idsWide,
      idsNarrow,
      "the set of placement ids must be identical at every viewport",
    );
    // Sanity: we expect every leaf to be placed.
    assert.equal(idsWide.length, 9, "demo doc has 9 leaves");
  } finally {
    solver.close();
  }
});
