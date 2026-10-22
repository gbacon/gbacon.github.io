import path from "node:path";
import sharp from "sharp";
import { OG_WIDTH, OG_HEIGHT } from "./og-size";

export interface ResolvedOgImage {
    src: string;
    width?: number;
    height?: number;
    alt?: string;
}

/**
 * Social preview for a content entry: front-matter `ogImage` (or legacy
 * `image`) if set, otherwise the card generated at /og/<collection>/<id>.png.
 */
export async function resolveOgImage(collection: string, id: string, data: any): Promise<ResolvedOgImage> {
    const custom: string | undefined = data.ogImage || data.image;
    if (custom) {
        const src = custom.startsWith("/") || /^https?:/.test(custom) ? custom : `/${custom}`;
        if (src.startsWith("/")) {
            try {
                const meta = await sharp(path.join(process.cwd(), "public", src)).metadata();
                return { src, width: meta.width, height: meta.height, alt: data.title };
            } catch {
                throw new Error(`ogImage ${src} for ${collection}/${id} not found under public/`);
            }
        }
        return { src, alt: data.title };
    }
    return { src: `/og/${collection}/${id}.png`, width: OG_WIDTH, height: OG_HEIGHT, alt: data.title };
}
