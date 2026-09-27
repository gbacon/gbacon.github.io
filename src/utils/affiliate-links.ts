export const PAID_LINK_TEXT = ' (paid link)';

export function isAmazonUrl(href: string): boolean {
  try {
    const { hostname } = new URL(href);

    return (
      hostname === 'amazon.com' ||
      hostname.endsWith('.amazon.com') ||
      hostname === 'amzn.to' ||
      hostname === 'link.amazon'
    );
  } catch {
    return false;
  }
}

export function externalLinkRel(href: string): string {
  const rel = ['noopener', 'noreferrer'];

  if (isAmazonUrl(href)) {
    rel.push('sponsored');
  }

  return rel.join(' ');
}
