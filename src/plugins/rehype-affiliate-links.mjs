import { visit } from 'unist-util-visit';
import {
  PAID_LINK_TEXT,
  isAmazonUrl,
  affiliateHref,
  externalLinkRel,
  hasAmazonDisclosure,
} from '../utils/affiliate-links.ts';

function textOf(node) {
  if (node.type === 'text') return node.value;
  return (node.children || []).map(textOf).join('');
}

/**
 * For every Amazon link in Markdown content:
 *  - rewrite amazon.com product URLs to https://www.amazon.com/dp/ASIN?tag=<configured tag>
 *  - append " (paid link)" and set rel="… sponsored"
 * Then record on the page's frontmatter (remarkPluginFrontmatter) how many
 * Amazon links it has and whether the text already carries the Associates
 * disclosure, so the layout can insert the disclosure near the top.
 */
export function affiliateLinkPlugin() {
  return (tree, file) => {
    let amazonLinks = 0;

    visit(tree, 'element', (node) => {
      if (node.tagName === 'a' && typeof node.properties?.href === 'string') {
        const href = node.properties.href;

        if (isAmazonUrl(href)) {
          amazonLinks += 1;
          node.properties.href = affiliateHref(href);

          const hasPaidText = node.children?.some(child =>
            typeof child.value === 'string' && child.value.includes(PAID_LINK_TEXT.trim())
          );

          if (!hasPaidText) {
            node.children.push({
              type: 'text',
              value: PAID_LINK_TEXT
            });
          }

          // Mark affiliate links as sponsored
          node.properties.rel = externalLinkRel(href);
        }
      }
    });

    const fm = file?.data?.astro?.frontmatter;
    if (fm) {
      fm.amazonLinks = amazonLinks;
      fm.amazonDisclosurePresent = hasAmazonDisclosure(textOf(tree));
    }
  };
}
