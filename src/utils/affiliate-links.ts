import { AMAZON_ASSOCIATES_TAG, AMAZON_DISCLOSURE } from '../config/affiliate.ts';

export { AMAZON_ASSOCIATES_TAG, AMAZON_DISCLOSURE };

export const PAID_LINK_TEXT = ' (paid link)';

// Hosts that serve Amazon links: amazon.com and its subdomains (www., smile.,
// m.), plus Amazon's own shorteners.
const AMAZON_SHORT_HOSTS = new Set(['amzn.to', 'amzn.com', 'a.co', 'link.amazon']);

function hostOf(href: string): string | null {
  try {
    return new URL(href).hostname.toLowerCase();
  } catch {
    return null;
  }
}

function isAmazonComHost(hostname: string): boolean {
  return hostname === 'amazon.com' || hostname.endsWith('.amazon.com');
}

export function isAmazonUrl(href: string): boolean {
  const hostname = hostOf(href);
  if (!hostname) return false;
  return isAmazonComHost(hostname) || AMAZON_SHORT_HOSTS.has(hostname);
}

const ASIN_PATTERNS = [
  /\/dp\/([A-Z0-9]{10})(?:[/?#]|$)/i,
  /\/gp\/product\/([A-Z0-9]{10})(?:[/?#]|$)/i,
  /\/gp\/aw\/d\/([A-Z0-9]{10})(?:[/?#]|$)/i,
  /\/exec\/obidos\/(?:ASIN|tg\/detail\/-)\/([A-Z0-9]{10})(?:[/?#]|$)/i,
];

/** The ASIN of an amazon.com product URL, or null (shortlinks, help pages, …). */
export function amazonAsin(href: string): string | null {
  const hostname = hostOf(href);
  if (!hostname || !isAmazonComHost(hostname)) return null;
  const { pathname } = new URL(href);
  for (const re of ASIN_PATTERNS) {
    const m = pathname.match(re);
    if (m) return m[1].toUpperCase();
  }
  return null;
}

/** Canonical tagged product URL: https://www.amazon.com/dp/ASIN?tag=<tag> */
export function amazonUrl(asin: string): string {
  return `https://www.amazon.com/dp/${asin.toUpperCase()}?tag=${AMAZON_ASSOCIATES_TAG}`;
}

/**
 * Rewrite any amazon.com product URL (SiteStripe, mobile, untagged /dp/ASIN,
 * …) to the canonical tagged form. Other URLs are returned unchanged.
 */
export function affiliateHref(href: string): string {
  const asin = amazonAsin(href);
  return asin ? amazonUrl(asin) : href;
}

export function externalLinkRel(href: string): string {
  const rel = ['noopener', 'noreferrer'];

  if (isAmazonUrl(href)) {
    rel.push('sponsored');
  }

  return rel.join(' ');
}

/** True if text already contains the Associates disclosure (any apostrophes/spacing). */
export function hasAmazonDisclosure(text: string): boolean {
  const norm = (s: string) => s.replace(/\s+/g, ' ').toLowerCase();
  return norm(text).includes(norm(AMAZON_DISCLOSURE).replace(/\.$/, ''));
}
