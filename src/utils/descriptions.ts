/**
 * Helpers for <meta name="description">. Search engines truncate snippets
 * past about 160 characters, and Bing flags descriptions shorter than ~120.
 */
export const DESCRIPTION_MAX = 160;

/** Append `suffix` to `base` only if the result still fits. */
export function appendIfFits(base: string, suffix: string, max = DESCRIPTION_MAX): string {
    return (base + suffix).length <= max ? base + suffix : base;
}

/**
 * Description for a tag page, built from the tag name and the titles of the
 * newest entries carrying it, e.g.
 *   4 posts and talks tagged “haskell” on Greg Bacon’s blog, including “A” and “B.”
 */
export function tagDescription(tag: string, items: { title: string; collection: string }[]): string {
    const n = items.length;
    const kinds = Array.from(new Set(items.map((i) => i.collection)));
    const nouns: Record<string, [string, string]> = {
        posts: ["post", "posts"],
        talks: ["talk", "talks"],
        publications: ["publication", "publications"],
        projects: ["project", "projects"],
        teaching: ["course", "courses"],
    };
    const noun =
        kinds.length === 1 && nouns[kinds[0]]
            ? nouns[kinds[0]][n === 1 ? 0 : 1]
            : n === 1 ? "entry" : "entries";
    const titles = items.map((i) => i.title.normalize("NFKC"));
    if (n === 1) {
        const base = `The one ${noun} tagged “${tag}” on Greg Bacon’s blog`;
        const withTitle = `${base}: “${titles[0]}.”`;
        let one = withTitle.length <= DESCRIPTION_MAX ? withTitle : `${base}.`;
        for (const s of [" More posts on code, AI/ML, aviation, and SBIR.", " Browse other topics on the tags page."]) {
            one = appendIfFits(one, s);
        }
        return one;
    }
    let desc = `${n} ${noun} tagged “${tag}” on Greg Bacon’s blog`;
    let added = 0;
    for (const t of titles) {
        const sep = added === 0 ? ", including " : ", ";
        const next = `${desc}${sep}“${t}”`;
        if (next.length + 1 > DESCRIPTION_MAX) break;
        desc = next;
        added++;
    }
    desc += ".";
    desc = appendIfFits(desc, " Browse them all, newest first.");
    desc = appendIfFits(desc, " Posts on code, AI/ML, aviation, and more.");
    return desc;
}
