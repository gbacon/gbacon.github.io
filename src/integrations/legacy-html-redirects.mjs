import { copyFile, readFile, readdir, rename, rm, stat, writeFile } from 'node:fs/promises';
import { join, relative, sep } from 'node:path';
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
 *
 * Astro's stub says "Redirecting from <code>/foo.html/</code>" because the
 * route was built with a trailing slash. Rewrite that visible source path to
 * the URL the stub is actually served at; the meta refresh and canonical
 * already point at the target and are left alone.
 *
 * For extensionless redirect sources such as /about, also write a sibling
 * dist/about.html copy of the dist/about/index.html stub. GitHub Pages serves
 * /about from about.html (200) when it can, saving the 301 to /about/; the
 * directory stub stays so /about/ keeps working.
 */
function fixSourceText(html, from) {
  return html.replace(
    /(Redirecting from <code>)[^<]*(<\/code>)/,
    (_, a, b) => `${a}${from}${b}`,
  );
}

async function exists(p) {
  try { await stat(p); return true; } catch { return false; }
}
export default function legacyHtmlRedirects() {
  let redirectSources = [];
  return {
    name: 'legacy-html-redirects',
    hooks: {
      'astro:config:done': ({ config }) => {
        redirectSources = Object.keys(config.redirects || {});
      },
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
                const from = '/' + relative(root, p).split(sep).join('/');
                await writeFile(p, fixSourceText(await readFile(p, 'utf8'), from));
                count++;
                continue;
              }
            }
            await walk(p);
          }
        }
        await walk(root);
        logger.info(`flattened ${count} legacy .html redirect(s)`);
        let siblings = 0;
        for (const from of redirectSources) {
          if (from.endsWith('.html') || /[[\]*]/.test(from)) continue;
          const name = from.replace(/^\/+|\/+$/g, '');
          const stub = join(root, name, 'index.html');
          if (!name || !(await exists(stub))) continue;
          const html = await readFile(stub, 'utf8');
          await writeFile(join(root, `${name}.html`), fixSourceText(html, `/${name}`));
          siblings++;
        }
        logger.info(`wrote ${siblings} extensionless redirect sibling(s)`);
        await copyFile(join(root, 'sitemap-index.xml'), join(root, 'sitemap.xml'));
        logger.info('copied sitemap-index.xml to sitemap.xml');
      },
    },
  };
}
