// @ts-check
import { defineConfig } from 'astro/config';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';
import sitemap from '@astrojs/sitemap';

import tailwindcss from '@tailwindcss/vite';

// https://astro.build/config
export default defineConfig({
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [rehypeKatex],
  },
  build: {
    inlineStylesheets: 'always'
  },
  vite: {
    plugins: [tailwindcss()],
  },
  site: 'https://blog.gbacon.com',
  // base: '/',
  integrations: [sitemap()],
  output: 'static',
  redirects: {
    "/2005/07/processing-lines-in-textbox.html": "/blog/processing-lines-in-textbox",
    "/2005/09/trigonometry-refactored.html": "/blog/trigonometry-refactored",
    "/2007/02/my-first-monadic-program.html": "/blog/my-first-monadic-program",
    "/2009/06/cleaning-up-your-haskell-imports.html": "/blog/cleaning-up-your-haskell-imports",
    "/2009/07/just-for-you-madeline.html": "/blog/just-for-you-madeline",
    "/2009/07/simple-fitnesse-example-with-cslim.html": "/blog/simple-fitnesse-example-with-cslim",
    "/2009/08/git-shrinking-subversion-import.html": "/blog/git-shrinking-subversion-import",
    "/2009/08/blog-post.html": "/blog/immanuel",
    "/2009/08/finding-duplicates-with-perl-and.html": "/blog/duplicates-perl-haskell",
    "/2010/03/perl-conditional-use-and-scope.html": "/blog/perl-conditional-use-and-scope",
    "/2011/01/checkers-game-over-in-haskell.html": "/blog/checkers-game-over-in-haskell",
    "/2011/03/extracting-comma-separated-integers.html": "/blog/extracting-comma-separated-integers",
    "/2011/04/considering-gas-powered-standby.html": "/blog/considering-gas-powered-standby",
    "/2011/05/netopenidconsumer-fails-with-naiveverifyfaile.html": "/blog/netopenidconsumer-fails-with-naiveverifyfaile",
  },
});