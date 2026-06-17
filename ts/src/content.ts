/**
 * Content-as-data: the TypeScript twin of the constructor's content JSON schema.
 *
 * Every interface here mirrors, field-for-field, a value the Haskell
 * `Swwstructor.Content` codec reads and writes (see
 * `src/Swwstructor/Content.hs`, `Theme.hs`, `Money.hs`). An admin editor can
 * therefore author a {@link SiteSpec} with full type-safety, and the JSON it
 * produces round-trips through the Haskell decoder.
 *
 * Conventions, taken from the codec:
 *   - Optional fields (`?:`) are exactly the Haskell `Maybe` fields: the encoder
 *     omits them when absent, and the decoder reads a missing key as absent.
 *   - `Price.amount` is an INTEGER count of minor units (cents); `currency` is a
 *     lower-case ISO code.
 *   - {@link SectionSpec} is a discriminated union on the `section` string tag —
 *     the same closed vocabulary as the Haskell `SectionSpec` sum.
 *
 * This module is rendering/editor glue only: it never describes layout geometry
 * (that is the engine's job, consumed via {@link "./preview.js"}).
 */

/* -------------------------------------------------------------------------- */
/* Leaf content values                                                        */
/* -------------------------------------------------------------------------- */

/**
 * A figure: an optional real image source, a caption, and the engine's two
 * sizing knobs. `aspect` = width / height; `cap` = max rendered height in px.
 * The cap is load-bearing — an uncapped aspect dominates its column.
 *
 * (Haskell `ImageRef`.) The decoder defaults `aspect` to 1.5 and `cap` to 360
 * when omitted, but both are required on the wire form the encoder emits, so we
 * keep them required here for editor clarity.
 */
export interface ImageRef {
  /** Real image URL; absent for a caption-only placeholder figure. */
  src?: string;
  /** Caption text (required, may be empty). */
  caption: string;
  /** Aspect ratio width / height. */
  aspect: number;
  /** Crop / max rendered height in px. */
  cap: number;
}

/** A call-to-action link. (Haskell `Cta`.) */
export interface Cta {
  label: string;
  href: string;
}

/**
 * A navigation link. (Haskell `NavLink` from `Swwstructor.Block`.) Shared by the
 * site nav, `navStrip`, and `ribbon` sections.
 */
export interface NavLink {
  label: string;
  href: string;
}

/** The supported settlement currencies, as their lower-case ISO codes. */
export type Currency = "eur" | "gbp" | "usd" | "jpy";

/**
 * A price: an integer `amount` in minor units (cents) tagged with a
 * {@link Currency}. (Haskell `Price`, where the amount is `Cents` and the
 * currency is the lower-case ISO code Stripe expects.) The amount is never
 * client-set for a real charge — the server resolves it from the buy target.
 */
export interface Price {
  /** Integer minor units (e.g. euro cents). */
  amount: number;
  currency: Currency;
}

/**
 * A sellable item. `buy` is the stable id a buy button posts to; the server
 * resolves it to this priced line item. (Haskell `Product`.)
 */
export interface Product {
  name: string;
  blurb: string;
  price: Price;
  image?: ImageRef;
  /** Stable buy-target id the buy button posts to. */
  buy: string;
}

/**
 * An article atom: the editorial unit of an NYT-style front. Everything but the
 * headline is optional, so a one-line link and a full lead story are the same
 * shape at different fill levels. (Haskell `Story`.)
 */
export interface Story {
  kicker?: string;
  headline: string;
  href?: string;
  dek?: string;
  byline?: string;
  timestamp?: string;
  image?: ImageRef;
  /**
   * Pin the image while its story's text is read (the figure-pinning case,
   * bounded by the story container). Omitted/false means not sticky.
   */
  imageSticky?: boolean;
  body?: string;
}

/* -------------------------------------------------------------------------- */
/* Sections                                                                   */
/* -------------------------------------------------------------------------- */

/** Centered wordmark + optional tagline. (Haskell `Masthead`.) */
export interface MastheadSection {
  section: "masthead";
  title: string;
  tagline?: string;
}

/** A nav bar; `sticky` pins it to the top for the whole page. (Haskell `NavStrip`.) */
export interface NavStripSection {
  section: "navStrip";
  links: NavLink[];
  sticky: boolean;
}

/** A thin link ribbon (the breaking-news strip). (Haskell `Ribbon`.) */
export interface RibbonSection {
  section: "ribbon";
  links: NavLink[];
}

/** A hero: copy column + figure rail. (Haskell `Hero`/`HeroContent`.) */
export interface HeroSection {
  section: "hero";
  kicker?: string;
  headline: string;
  dek?: string;
  body?: string;
  image?: ImageRef;
  cta?: Cta;
}

/**
 * The two-column front: `rho` (main column weight), `gutter` (px), the main
 * stories and the rail stories. Re-homes to one column on a phone.
 * (Haskell `FeatureSplit`.)
 */
export interface FeatureSplitSection {
  section: "featureSplit";
  rho: number;
  gutter: number;
  main: Story[];
  rail: Story[];
}

/** An equal-weight row of stories. (Haskell `StoryRow`.) */
export interface StoryRowSection {
  section: "storyRow";
  stories: Story[];
}

/** A product grid: optional title, optional intro, the catalogue. (Haskell `ProductGrid`.) */
export interface ProductGridSection {
  section: "productGrid";
  title?: string;
  intro?: string;
  products: Product[];
}

/** N prose columns under an optional title. (Haskell `RichColumns`.) */
export interface RichColumnsSection {
  section: "richColumns";
  title?: string;
  columns: string[];
}

/** An image gallery under an optional title. (Haskell `Gallery`.) */
export interface GallerySection {
  section: "gallery";
  title?: string;
  images: ImageRef[];
}

/** A call-to-action band: headline, optional body, optional button. (Haskell `CtaBand`.) */
export interface CtaBandSection {
  section: "ctaBand";
  headline: string;
  body?: string;
  cta?: Cta;
}

/** Contact: title, optional body, optional email. (Haskell `Contact`.) */
export interface ContactSection {
  section: "contact";
  title: string;
  body?: string;
  email?: string;
}

/**
 * A generic prose block: optional kicker, optional headline, body. (Haskell
 * `ProseSection`, whose wire tag is `"prose"`.)
 */
export interface ProseSection {
  section: "prose";
  kicker?: string;
  headline?: string;
  body: string;
}

/** A footer band. (Haskell `FooterBand`, whose wire tag is `"footer"`.) */
export interface FooterSection {
  section: "footer";
  text: string;
}

/**
 * The closed set of section kinds — a discriminated union on the `section` tag.
 * Each maps to a template in the Haskell `Swwstructor.Templates`. This is the
 * vocabulary an owner writes. (Haskell `SectionSpec`.)
 */
export type SectionSpec =
  | MastheadSection
  | NavStripSection
  | RibbonSection
  | HeroSection
  | FeatureSplitSection
  | StoryRowSection
  | ProductGridSection
  | RichColumnsSection
  | GallerySection
  | CtaBandSection
  | ContactSection
  | ProseSection
  | FooterSection;

/** The literal `section` tags, for editor pickers and exhaustiveness checks. */
export type SectionTag = SectionSpec["section"];

/* -------------------------------------------------------------------------- */
/* Theme                                                                      */
/* -------------------------------------------------------------------------- */

/** Vertical rhythm. `airy` is generous (editorial); `compact` is dense. */
export type Density = "airy" | "cozy" | "compact";

/**
 * A theme: palette, typography, density, and geometry. Branding lives in a
 * value, not in hard-coded CSS. (Haskell `Theme`.)
 *
 * Every field is optional here because the Haskell decoder falls back to the
 * default theme field-by-field — a partial theme object is always valid, and an
 * editor only overrides what it cares about. `fontUrl` may be explicitly `null`
 * to mean "no webfont" (the codec encodes `Nothing` as JSON `null`).
 */
export interface Theme {
  name?: string;
  /** Background colour (CSS colour literal). */
  bg?: string;
  /** Foreground / text colour. */
  fg?: string;
  /** Accent colour. */
  accent?: string;
  /** Muted / secondary text colour. */
  muted?: string;
  /** Hairline / rule colour. */
  border?: string;
  /** Font stack for the masthead wordmark. */
  mastFont?: string;
  /** Font stack for headlines. */
  displayFont?: string;
  /** Font stack for body copy. */
  bodyFont?: string;
  /** Optional webfont stylesheet href; `null` for none. */
  fontUrl?: string | null;
  density?: Density;
  /** Max content width in px. */
  maxWidth?: number;
  /** Corner radius (px) for figures and buttons; 0 for a hard newspaper look. */
  radius?: number;
}

/* -------------------------------------------------------------------------- */
/* Pages + site                                                               */
/* -------------------------------------------------------------------------- */

/** A page: its path (`"/"`, `"/classes"`…), a title, and its sections. (Haskell `PageSpec`.) */
export interface PageSpec {
  path: string;
  title: string;
  sections: SectionSpec[];
}

/**
 * A whole site: metadata, theme, shared nav, footer, and pages. (Haskell
 * `SiteSpec`.) `description` and `theme` are optional here — the decoder fills
 * them with `""` and the default theme respectively when absent.
 */
export interface SiteSpec {
  title: string;
  description?: string;
  baseUrl?: string;
  theme?: Theme;
  nav: NavLink[];
  footer?: string;
  pages: PageSpec[];
}

/* -------------------------------------------------------------------------- */
/* Runtime helpers                                                            */
/* -------------------------------------------------------------------------- */

/**
 * A fresh, empty site: a single home page at `"/"` with no sections and an empty
 * nav. The minimal value an editor can start from; it satisfies
 * {@link isSiteSpec}.
 */
export function emptySite(): SiteSpec {
  return {
    title: "",
    nav: [],
    pages: [{ path: "/", title: "Home", sections: [] }],
  };
}

/**
 * Shallow structural guard: is `x` a plausible {@link SiteSpec}? Checks the two
 * load-bearing required fields — a string `title` and an array of `pages` — so
 * that JSON loaded from disk or the wire can be narrowed before use. It does NOT
 * deeply validate every section (the Haskell decoder is the authority for that);
 * it is a cheap fast-fail for an editor.
 */
export function isSiteSpec(x: unknown): x is SiteSpec {
  if (typeof x !== "object" || x === null) return false;
  const o = x as Record<string, unknown>;
  return typeof o.title === "string" && Array.isArray(o.pages);
}
