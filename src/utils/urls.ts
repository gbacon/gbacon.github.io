/**
 * GitHub Pages serves /foo/index.html at /foo/ and 301-redirects /foo to it,
 * so every internal page URL should end in a slash (astro.config.mjs sets
 * trailingSlash: 'always'). Paths with a file extension (/rss.xml,
 * /og/posts/x.png, legacy /2009/08/x.html) and #/? suffixes are preserved.
 */
export function withTrailingSlash(href: string): string {
    const m = href.match(/^([^?#]*)([?#].*)?$/);
    if (!m) return href;
    const [, path, suffix = ""] = m;
    if (path === "" || path.endsWith("/")) return href;
    const last = path.slice(path.lastIndexOf("/") + 1);
    if (/\.[A-Za-z0-9]+$/.test(last)) return href;
    return `${path}/${suffix}`;
}
