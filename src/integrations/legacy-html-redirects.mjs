import { copyFile, readdir, rename, rm } from 'node:fs/promises';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

/**
 * With trailingSlash: 'always', Astro writes a redirect for an old Blogger URL
 * such as /2010/03/foo.html to dist/2010/03/foo.html/index.html. GitHub Pages
 * would then answer /2010/03/foo.html with an extra 301 to /2010/03/foo.html/.
 * Flatten each such directory back into a plain file so the old URL serves the
 * redirect stub (meta refresh + canonical + noindex) directly.
 *
 * Also publish the sitemap index at the old Jekyll /sitemap.xml URL: a meta
 * refresh is useless to a crawler fetching XML, so serve the real thing.
 * List this integration after sitemap() so sitemap-index.xml already exists.
 */
export default function legacyHtmlRedirects() {
  return {
    name: 'legacy-html-redirects',
    hooks: {
      'astro:build:done': async ({ dir, logger }) => {
        const root = fileURLToPath(dir);
        let count = 0;
        async function walk(d) {
          for (const ent of await readdir(d, { withFileTypes: true })) {
            if (!ent.isDirectory()) continue;
            const p = join(d, ent.name);
            if (ent.name.endsWith('.html')) {
              const entries = await readdir(p);
              if (entries.length === 1 && entries[0] === 'index.html') {
                const tmp = `${p}.tmp`;
                await rename(join(p, 'index.html'), tmp);
                await rm(p, { recursive: true });
                await rename(tmp, p);
                count++;
                continue;
              }
            }
            await walk(p);
          }
        }
        await walk(root);
        logger.info(`flattened ${count} legacy .html redirect(s)`);
        await copyFile(join(root, 'sitemap-index.xml'), join(root, 'sitemap.xml'));
        logger.info('copied sitemap-index.xml to sitemap.xml');
      },
    },
  };
}
