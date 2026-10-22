import type { APIRoute, GetStaticPaths } from "astro";
import { getCollection } from "astro:content";
import { renderOgPng } from "../../utils/og";

// One 1200×630 card per content entry, at /og/<collection>/<id>.png.
const COLLECTIONS = {
    posts: "Post",
    talks: "Talk",
    publications: "Publication",
    projects: "Project",
    teaching: "Teaching",
} as const;

function formatDate(d?: string) {
    if (!d) return undefined;
    const date = new Date(d);
    return isNaN(date.getTime())
        ? undefined
        : date.toLocaleDateString("en-US", { year: "numeric", month: "long", timeZone: "UTC" });
}

export const getStaticPaths: GetStaticPaths = async () => {
    const paths = [];
    for (const [collection, kicker] of Object.entries(COLLECTIONS)) {
        for (const entry of await getCollection(collection as keyof typeof COLLECTIONS)) {
            const d = entry.data as any;
            paths.push({
                params: { slug: `${collection}/${entry.id}` },
                props: { title: d.title, kicker, date: formatDate(d.date), tags: d.tags },
            });
        }
    }
    return paths;
};

export const GET: APIRoute = async ({ props }) => {
    const png = await renderOgPng(props as any);
    return new Response(new Uint8Array(png), { headers: { "Content-Type": "image/png" } });
};
