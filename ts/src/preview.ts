/**
 * The live layout preview.
 *
 * The constructor server is the single source of layout truth: it solves each
 * page with the `stickywebwm` engine and exposes the resulting wire
 * {@link Document} at `GET /admin/preview.json?path=<pagePath>`. This module
 * fetches that Document and hands it to the SDK's {@link mount}, which solves it
 * (via the supplied {@link Solver}), absolutely-positions the boxes, re-solves on
 * width change, and drives sticky — we add NO second solver of our own.
 *
 * The admin page wiring (browser):
 *
 * ```ts
 * import { loadBundle, BundleSolver } from "stickywebwm";          // or via swwstructor re-exports
 * import { mountPreview } from "swwstructor";
 *
 * // 1. load the GHC-JS solver bundle (the Nix `solver-js` output) onto a global
 * await import("/static/stickywebwm-solver.js");
 * // 2. wrap it as a Solver
 * const solver = loadBundle(globalThis); // BundleSolver
 * // 3. mount a live, responsive, sticky preview of a page
 * const app = await mountPreview(document.getElementById("preview")!, {
 *   path: "/",
 *   solver,
 * });
 * // ... later: app.destroy();
 * ```
 *
 * Security: the preview only ever fetches a layout {@link Document} — never any
 * key or secret. Nothing here puts a secret in the bundle.
 */

import { mount } from "stickywebwm";
import type {
  Document,
  Placement,
  Solver,
  SolveResponse,
  Viewport,
} from "stickywebwm";

/**
 * Re-export of the SDK surface an admin page needs to bootstrap the preview, so
 * the page can import everything from `swwstructor` alone:
 *
 * ```ts
 * import { loadBundle, BundleSolver, mountPreview } from "swwstructor";
 * ```
 */
export {
  loadBundle,
  BundleSolver,
  NativeSolver,
  SolverError,
  mount,
} from "stickywebwm";
export type {
  Solver,
  Document,
  Placement,
  Viewport,
  SolveResponse,
} from "stickywebwm";

/** Options for {@link mountPreview}. */
export interface MountPreviewOptions {
  /** The page path to preview (e.g. `"/"`, `"/classes"`). */
  path: string;
  /**
   * Base URL of the constructor server. Defaults to the empty string (same
   * origin), so the admin page fetches its own server's `/admin/preview.json`.
   */
  baseUrl?: string;
  /** The solver transport to drive (a {@link BundleSolver} in the browser). */
  solver: Solver;
  /**
   * Optional override for the global `fetch` (mainly for tests). Defaults to the
   * ambient `fetch`.
   */
  fetchImpl?: typeof fetch;
  /** Reserved gap hint forwarded to the sticky step (hysteresis). */
  gap?: number;
  /** Px from the top of the viewport at which a pinned window fixes. */
  pinOffset?: number;
}

/** Handle returned by {@link mountPreview}. */
export interface PreviewHandle {
  /** Tear down the mounted preview (disconnect observers, clear the DOM). */
  destroy(): void;
  /**
   * Re-fetch the page's Document and re-mount it. Use after an edit changes the
   * page's layout; for a pure width change the SDK already re-solves on its own.
   */
  refresh(): Promise<void>;
}

/** Build the preview endpoint URL for a page path under an optional base URL. */
export function previewUrl(path: string, baseUrl = ""): string {
  // `path` is a server-side page path; `encodeURIComponent` keeps a `/` or query
  // char in it from corrupting our own query string.
  return `${baseUrl}/admin/preview.json?path=${encodeURIComponent(path)}`;
}

/** Narrow a parsed JSON value to a stickywebwm wire {@link Document}. */
function isDocument(v: unknown): v is Document {
  if (typeof v !== "object" || v === null) return false;
  const t = (v as { t?: unknown }).t;
  return t === "leaf" || t === "stack" || t === "splitN";
}

/**
 * Fetch the solved layout {@link Document} for a page from the constructor
 * server's `/admin/preview.json`. Throws on a non-OK response or a body that is
 * not a recognisable Document.
 */
export async function fetchPreviewDocument(
  path: string,
  baseUrl = "",
  fetchImpl: typeof fetch = fetch,
): Promise<Document> {
  const url = previewUrl(path, baseUrl);
  const res = await fetchImpl(url, {
    headers: { accept: "application/json" },
  });
  if (!res.ok) {
    throw new Error(
      `preview fetch failed: ${res.status} ${res.statusText} for ${url}`,
    );
  }
  const body: unknown = await res.json();
  if (!isDocument(body)) {
    throw new Error(`preview fetch returned a non-Document body for ${url}`);
  }
  return body;
}

/**
 * The default preview {@link Placement} renderer: label each placed box with its
 * window id and type, so the preview is readable without any real content. The
 * id is the prominent line; the type is a quieter tag.
 */
export function renderPreviewBox(p: Placement, el: HTMLElement): void {
  el.classList.add("sww-preview-box");
  el.style.outline = "1px solid rgba(0,0,0,.18)";
  el.style.overflow = "hidden";
  el.style.font = "12px/1.3 system-ui, sans-serif";
  el.style.padding = "4px 6px";
  el.style.background = "rgba(0,0,0,.02)";
  // Rebuild contents each solve (cheap; placements are small).
  el.textContent = "";
  const idEl = document.createElement("strong");
  idEl.textContent = p.id;
  const typeEl = document.createElement("span");
  typeEl.style.opacity = "0.55";
  typeEl.style.marginLeft = "6px";
  typeEl.textContent = p.type;
  el.appendChild(idEl);
  el.appendChild(typeEl);
}

/**
 * Mount a live, responsive, sticky preview of a page into `root`.
 *
 * Fetches the page's wire {@link Document} from
 * `${baseUrl}/admin/preview.json?path=<path>` and hands it to the SDK's
 * {@link mount} with a {@link renderPreviewBox} render callback. The returned
 * handle's `refresh()` re-fetches and re-mounts (for after an edit); width-driven
 * re-solving is handled by the SDK runtime itself.
 */
export async function mountPreview(
  root: HTMLElement,
  opts: MountPreviewOptions,
): Promise<PreviewHandle> {
  const baseUrl = opts.baseUrl ?? "";
  const fetchImpl = opts.fetchImpl ?? fetch;

  const makeMount = async () => {
    const doc = await fetchPreviewDocument(opts.path, baseUrl, fetchImpl);
    return mount(root, doc, {
      solver: opts.solver,
      render: renderPreviewBox,
      ...(opts.gap !== undefined ? { gap: opts.gap } : {}),
      ...(opts.pinOffset !== undefined ? { pinOffset: opts.pinOffset } : {}),
    });
  };

  let handle = await makeMount();

  return {
    destroy(): void {
      handle.destroy();
    },
    async refresh(): Promise<void> {
      // A new Document may have a different tree, so we fully re-mount rather than
      // re-solve the old doc. Tear the old mount down first to avoid stray boxes.
      handle.destroy();
      handle = await makeMount();
    },
  };
}

/* -------------------------------------------------------------------------- */
/* Node-testable helper                                                        */
/* -------------------------------------------------------------------------- */

/**
 * Solve a {@link Document} at an explicit `width` and {@link Viewport} and return
 * just the {@link Placement}s — a DOM-free helper so tests can assert
 * conservation and responsiveness without mounting anything. This is a thin pass
 * through to `solver.solve`; it adds no layout logic (the engine remains the
 * single source of truth).
 */
export async function solvePreview(
  doc: Document,
  solver: Solver,
  width: number,
  viewport: Viewport,
): Promise<Placement[]> {
  const resp: SolveResponse = await solver.solve({
    document: doc,
    viewport,
    width,
  });
  return resp.placements;
}
