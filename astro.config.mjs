// @ts-check
import { defineConfig } from 'astro/config';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import sitemap from '@astrojs/sitemap';

import tailwindcss from '@tailwindcss/vite';

import { affiliateLinkPlugin } from './src/plugins/rehype-affiliate-links.mjs';
import { trailingSlashLinkPlugin } from './src/plugins/rehype-trailing-slash.mjs';
import legacyHtmlRedirects from './src/integrations/legacy-html-redirects.mjs';

// https://astro.build/config
export default defineConfig({
  // GitHub Pages serves directory index.html files at /path/ and 301s /path to it.
  trailingSlash: 'always',
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [
      rehypeKatex,
      affiliateLinkPlugin,
      trailingSlashLinkPlugin,
    ],
  },
  build: {
    inlineStylesheets: 'always'
  },
  vite: {
    plugins: [tailwindcss()],
  },
  site: 'https://blog.gbacon.com',
  // base: '/',
  integrations: [
    sitemap({
      changefreq: 'weekly',
      priority: 0.7,
      lastmod: new Date(),
      filter: (page) => {
        return !page.includes('/404') &&
               !page.match(/\/posts\/\d+\/?$/) &&
               !page.includes('/dev-tools/') &&
               !page.match(/\/projects\/thesus-mouse\/?$/);
      },
    }),
    legacyHtmlRedirects(),
  ],
  output: 'static',
  redirects: {
    "/about": "/",
    "/articles": "/posts/",
    "/2005/07/processing-lines-in-textbox.html": "/posts/processing-lines-in-textbox/",
    "/2005/09/trigonometry-refactored.html": "/posts/trigonometry-refactored/",
    "/2006/10/moral-regular-expression-is-poor.html": "/posts/regex-poor-substitute-for-parser/",
    "/2007/02/my-first-monadic-program.html": "/posts/my-first-monadic-program/",
    "/2007/03/processing-lines-in-textbox.html": "/posts/processing-lines-in-textbox/",
    "/2007/03/walk-right-in-its-around-back-just-half.html": "/posts/around-back/",
    "/2007/04/simple-obvious-truth.html": "/",
    "/2007/05/i-seen-it.html": "/posts/i-seen-it/",
    "/2007/06/word-ladder.html": "/posts/word-ladder-lisp/",
    "/2007/08/word-ladder-in-haskell.html": "/posts/word-ladder-haskell/",
    "/2007/08/word-ladder-in-python.html": "/posts/word-ladder-python/",
    "/2007/09/dear-coach-saban.html": "/posts/just-beat-auburn/",
    "/2007/11/how-to-lose-six-in-row-to-cow-college.html": "/posts/how-to-lose/",
    "/2008/06/dear-coach-saban.html": "/posts/dear-coach-saban-lose-shula-players/",
    "/2008/12/sec-coaches-spoof.html": "/posts/sec-coaches-spoof/",
    "/2008/12/whose-rev-is-it-anyway.html": "/posts/whose-rev-is-it-anyway/",
    "/2009/02/scraping-data-from-programs-output.html": "/posts/scraping-data-from-program-output/",
    "/2009/06/cleaning-up-your-haskell-imports.html": "/posts/cleaning-up-your-haskell-imports/",
    "/2009/06/rip-heather.html": "/posts/rip-heather/",
    "/2009/06/setting-up-simple-test-with-cabal.html": "/posts/simple-test-cabal/",
    "/2009/06/waiting-for-go-done.html": "/posts/waiting-for-go-done/",
    "/2009/07/gyrigrams.html": "/posts/gyrigrams/",
    "/2009/07/if-code-monkey-thing-doesnt-work-out.html": "/posts/radio-host-wbhp/",
    "/2009/07/just-for-you-madeline.html": "/posts/just-for-you-madeline/",
    "/2009/07/simple-fitnesse-example-with-cslim.html": "/posts/simple-fitnesse-example-with-cslim/",
    "/2009/08/git-shrinking-subversion-import.html": "/posts/git-shrinking-subversion-import/",
    "/2009/08/blog-post.html": "/posts/immanuel/",
    "/2009/08/finding-duplicates-with-perl-and.html": "/posts/duplicates-perl-haskell/",
    "/2010/03/perl-conditional-use-and-scope.html": "/posts/perl-conditional-use-and-scope/",
    "/2010/12/checkers-game-over-in-haskell.html": "/posts/checkers-game-over-in-haskell/",
    "/2011/01/checkers-game-over-in-haskell.html": "/posts/checkers-game-over-in-haskell/",
    "/2011/02/extracting-comma-separated-integers.html": "/posts/extracting-comma-separated-integers/",
    "/2011/03/extracting-comma-separated-integers.html": "/posts/extracting-comma-separated-integers/",
    "/2011/04/considering-gas-powered-standby.html": "/posts/considering-gas-powered-standby/",
    "/2011/05/netopenidconsumer-fails-with-naiveverifyfaile.html": "/posts/netopenidconsumer-fails-with-naiveverifyfaile/",
  },
});
