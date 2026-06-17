/**
 * swwstructor — the TypeScript admin/preview client for the `swwstructor`
 * website constructor.
 *
 * Two halves:
 *   - {@link "./content.js" | content}: type-safe interfaces mirroring the
 *     constructor's content JSON schema, for a type-safe admin editor.
 *   - {@link "./preview.js" | preview}: a live, responsive, sticky layout preview
 *     that consumes the `stickywebwm` TypeScript SDK. The constructor server
 *     solves each page; this client only fetches the resulting layout
 *     {@link Document} and renders it — never a second solver.
 *
 * @example Admin page bootstrap (browser)
 * ```ts
 * import { loadBundle, mountPreview, emptySite } from "swwstructor";
 *
 * await import("/static/stickywebwm-solver.js"); // GHC-JS bundle -> global
 * const solver = loadBundle(globalThis);
 * const app = await mountPreview(document.getElementById("preview")!, {
 *   path: "/",
 *   solver,
 * });
 * ```
 */

export * from "./content.js";
export * from "./preview.js";
