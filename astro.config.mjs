// @ts-check
import { defineConfig } from 'astro/config';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import sitemap from '@astrojs/sitemap';

import tailwindcss from '@tailwindcss/vite';

import { affiliateLinkPlugin } from './src/plugins/remark-affiliate-links.mjs';

// https://astro.build/config
export default defineConfig({
  trailingSlash: 'never',
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [
      rehypeKatex,
      affiliateLinkPlugin,
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
               !page.includes('/dev-tools/');
      },
    })
  ],
  output: 'static',
  redirects: {
    "/about": "/",
    "/articles": "/posts",
    "/sitemap.xml": "/sitemap-index.xml",
    "/2005/07/processing-lines-in-textbox.html": "/posts/processing-lines-in-textbox",
    "/2005/09/trigonometry-refactored.html": "/posts/trigonometry-refactored",
    "/2007/02/my-first-monadic-program.html": "/posts/my-first-monadic-program",
    "/2008/12/whose-rev-is-it-anyway.html": "/posts/whose-rev-is-it-anyway",
    "/2009/06/cleaning-up-your-haskell-imports.html": "/posts/cleaning-up-your-haskell-imports",
    "/2009/06/setting-up-simple-test-with-cabal.html": "/posts/simple-test-cabal",
    "/2009/07/just-for-you-madeline.html": "/posts/just-for-you-madeline",
    "/2009/07/simple-fitnesse-example-with-cslim.html": "/posts/simple-fitnesse-example-with-cslim",
    "/2009/08/git-shrinking-subversion-import.html": "/posts/git-shrinking-subversion-import",
    "/2009/08/blog-post.html": "/posts/immanuel",
    "/2009/08/finding-duplicates-with-perl-and.html": "/posts/duplicates-perl-haskell",
    "/2010/03/perl-conditional-use-and-scope.html": "/posts/perl-conditional-use-and-scope",
    "/2011/01/checkers-game-over-in-haskell.html": "/posts/checkers-game-over-in-haskell",
    "/2011/03/extracting-comma-separated-integers.html": "/posts/extracting-comma-separated-integers",
    "/2011/04/considering-gas-powered-standby.html": "/posts/considering-gas-powered-standby",
    "/2011/05/netopenidconsumer-fails-with-naiveverifyfaile.html": "/posts/netopenidconsumer-fails-with-naiveverifyfaile",
  },
});
