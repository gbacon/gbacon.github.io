import { visit } from 'unist-util-visit';
import { withTrailingSlash } from '../utils/urls.ts';

const SITE_HOSTS = new Set(['blog.gbacon.com']);

/**
 * Add the trailing slash GitHub Pages expects to internal page links in
 * Markdown (root-relative /posts/foo and absolute https://blog.gbacon.com/...),
 * so readers and crawlers never hit the /foo -> /foo/ redirect.
 */
export function trailingSlashLinkPlugin() {
  return (tree) => {
    visit(tree, 'element', (node) => {
      if (node.tagName !== 'a') return;
      const href = node.properties?.href;
      if (typeof href !== 'string') return;
      if (href.startsWith('/') && !href.startsWith('//')) {
        node.properties.href = withTrailingSlash(href);
        return;
      }
      try {
        const u = new URL(href);
        if ((u.protocol === 'https:' || u.protocol === 'http:') && SITE_HOSTS.has(u.hostname)) {
          node.properties.href = u.origin + withTrailingSlash(u.pathname + u.search + u.hash);
        }
      } catch {
        /* relative or non-URL href: leave it alone */
      }
    });
  };
}
