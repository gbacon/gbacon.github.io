import type { PagesConfig } from "../types";

export const NOT_FOUND_DESCRIPTION =
    "Page not found on blog.gbacon.com. Head back to the home page or browse Greg Bacon’s posts on programming, AI/ML, aviation, and SBIR/STTR proposals.";

export const PAGES: PagesConfig = {
    home: {
        title: "About Me",
        subtitle: "",
        description:
            "Greg Bacon, Ph.D. computer scientist in Madison, Alabama: AI/ML and software engineering, OASYS CTO, UAH adjunct, and CFI. Posts on code, aviation, and more.",
        isActive: true,
    },
    blog: {
        title: "Blog",
        subtitle: "Thoughts and observations.",
        description:
            "Greg Bacon’s blog: Perl, Haskell, and regex how-tos, AI/ML, SBIR/STTR proposals, flight instruction and checkrides, and classic Exponent columns.",
        isActive: true,
    },
    publications: {
        title: "Publications",
        subtitle: "A collection of research papers, books, and scientific articles.",
        description:
            "Publications by Greg Bacon: IEEE SoutheastCon papers on LLMs and AI/ML, Perl Journal articles, and Perl and Windows books he wrote for or technically edited.",
        isActive: true,
    },
    talks: {
        title: "Talks & Presentations",
        subtitle: "Public lectures and conference presentations.",
        description:
            "Talks by Greg Bacon on SBIR/STTR proposals, theoretical CS and the Cracker Barrel peg game, Haskell type inference, Perl objects, and Shuttle V&V.",
        isActive: true,
    },
    projects: {
        title: "Code & Projects",
        subtitle: "Open source contributions and technological experiments.",
        description:
            "Code and projects by Greg Bacon, including a Rust and WebAssembly reimagining of Claude Shannon’s 1950 Theseus maze-solving mouse that learns as you watch.",
        isActive: true,
    },
    teaching: {
        title: "Teaching",
        subtitle: "Academic courses and educational materials.",
        description:
            "Courses Greg Bacon teaches at the University of Alabama in Huntsville: algorithms (CS317) and formal languages and automata theory (CS403 and CS603).",
        isActive: true,
    },
    tags: {
        title: "Tags",
        subtitle: "Explore content by topic.",
        description:
            "Browse Greg Bacon’s posts, talks, publications, and projects by topic, from Perl, Haskell, and regex to AI/ML, aviation, SBIR, and the Exponent columns.",
        isActive: true,
    },
    cv: {
        title: "Curriculum Vitae",
        subtitle: "Academic and professional history.",
        description:
            "Curriculum vitae of Greg Bacon, Ph.D.: CTO at OASYS, UAH computer science lecturer, and flight instructor, with education, research, publications, and talks.",
        isActive: true,
    },
    disclaimers: {
        title: "Disclaimers & Disclosures",
        subtitle: "Legal notices and affiliate disclosures",
        description:
            "Disclaimers and disclosures for blog.gbacon.com: Amazon Associates affiliate links, Google Analytics, warranty and liability limits, and the privacy policy.",
        isActive: true,
    },
};
