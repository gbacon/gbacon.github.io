/**
 * Build-time Open Graph card renderer: satori lays out the card as SVG,
 * resvg rasterizes it to a 1200×630 PNG. Colors follow the light_default
 * theme in src/config/themes.ts; fonts are the site's Inter and JetBrains Mono.
 */
import fs from "node:fs";
import path from "node:path";
import satori from "satori";
import { Resvg } from "@resvg/resvg-js";
import { THEMES } from "../config";

import { OG_WIDTH, OG_HEIGHT } from "./og-size";

const theme = THEMES.light_default;
const root = process.cwd();
const font = (pkg: string, file: string) =>
    fs.readFileSync(path.join(root, "node_modules", "@fontsource", pkg, "files", file));

let fonts: any[] | undefined;
function loadFonts() {
    fonts ??= [
        { name: "Inter", data: font("inter", "inter-latin-400-normal.woff"), weight: 400, style: "normal" },
        { name: "Inter", data: font("inter", "inter-latin-700-normal.woff"), weight: 700, style: "normal" },
        { name: "Inter", data: font("inter", "inter-latin-800-normal.woff"), weight: 800, style: "normal" },
        { name: "Inter", data: font("inter", "inter-latin-ext-800-normal.woff"), weight: 800, style: "normal" },
        { name: "JetBrains Mono", data: font("jetbrains-mono", "jetbrains-mono-latin-500-normal.woff"), weight: 500, style: "normal" },
    ];
    return fonts;
}

let avatar: string | undefined;
function loadAvatar() {
    avatar ??= `data:image/jpeg;base64,${fs.readFileSync(path.join(root, "public", "greg.jpg")).toString("base64")}`;
    return avatar;
}

// Minimal hyperscript so we don't need JSX in a .ts endpoint.
type Node = { type: string; props: Record<string, any> };
const h = (type: string, style: Record<string, any>, ...children: any[]): Node => ({
    type,
    props: { style, children: children.length === 1 ? children[0] : children },
});

function titleSize(title: string): number {
    const n = title.length;
    if (n <= 30) return 76;
    if (n <= 55) return 66;
    if (n <= 80) return 56;
    return 48;
}

export interface OgCard {
    title: string;
    kicker?: string; // e.g. "Post", "Talk"
    date?: string;
    tags?: string[];
}

export async function renderOgPng(card: OgCard): Promise<Buffer> {
    // Fancy Unicode (e.g. math italic letters) has no glyphs in Inter; fold it.
    const title = card.title.normalize("NFKC");
    const meta = [card.date, ...(card.tags ?? []).slice(0, 3).map((t) => `#${t.toLowerCase()}`)]
        .filter(Boolean)
        .join("  ·  ");

    const tree = h(
        "div",
        {
            width: "100%",
            height: "100%",
            display: "flex",
            flexDirection: "column",
            backgroundColor: theme.background,
            fontFamily: "Inter",
            color: theme.foreground,
        },
        h("div", { display: "flex", width: "100%", height: 14, backgroundColor: theme.accent }),
        h(
            "div",
            {
                display: "flex",
                flexDirection: "column",
                justifyContent: "space-between",
                flexGrow: 1,
                padding: "56px 72px 56px 72px",
            },
            h(
                "div",
                {
                    display: "flex",
                    fontFamily: "JetBrains Mono",
                    fontSize: 28,
                    color: theme.accent,
                    letterSpacing: 1,
                },
                card.kicker ? `blog.gbacon.com / ${card.kicker.toLowerCase()}` : "blog.gbacon.com",
            ),
            h(
                "div",
                {
                    display: "flex",
                    fontSize: titleSize(title),
                    fontWeight: 800,
                    lineHeight: 1.12,
                    letterSpacing: -1,
                    maxHeight: 360,
                    overflow: "hidden",
                },
                title,
            ),
            h(
                "div",
                {
                    display: "flex",
                    alignItems: "center",
                    borderTop: `2px solid ${theme.border}`,
                    paddingTop: 28,
                },
                {
                    type: "img",
                    props: {
                        src: loadAvatar(),
                        width: 64,
                        height: 64,
                        style: { borderRadius: 32, marginRight: 20 },
                    },
                },
                h(
                    "div",
                    { display: "flex", flexDirection: "column" },
                    h("div", { display: "flex", fontSize: 28, fontWeight: 700 }, "Greg Bacon"),
                    meta ? h("div", { display: "flex", fontSize: 22, color: theme.muted, marginTop: 4 }, meta) : "",
                ),
            ),
        ),
    );

    const svg = await satori(tree as any, { width: OG_WIDTH, height: OG_HEIGHT, fonts: loadFonts() });
    return new Resvg(svg, {
        fitTo: { mode: "width", value: OG_WIDTH },
        // satori already converts text to paths; skipping the system font scan is ~8x faster.
        font: { loadSystemFonts: false },
    }).render().asPng();
}
