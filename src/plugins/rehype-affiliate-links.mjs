import { visit } from 'unist-util-visit';
import {
  PAID_LINK_TEXT,
  isAmazonUrl,
  externalLinkRel,
} from '../utils/affiliate-links.ts';

export function affiliateLinkPlugin() {
  return (tree) => {
    visit(tree, 'element', (node) => {
      if (node.tagName === 'a' && node.properties?.href) {
        const href = node.properties.href;

        if (isAmazonUrl(href)) {
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
  };
}
