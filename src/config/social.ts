import type { SocialLink } from "../types";

export const SOCIALS: SocialLink[] = [
    {
        name: "Github",
        href: "https://github.com/gbacon",
        linkTitle: `Follow Greg on Github`,
        isActive: true,
    },
    {
        name: "Mail",
        href: "mailto:gbacon@gbacon.com",
        linkTitle: `Email Greg`,
        isActive: true,
    },
    {
        name: "Google Scholar",
        href: "https://scholar.google.com/citations?user=erlO700AAAAJ",
        linkTitle: `Greg Bacon on Google Scholar`,
        isActive: true,
    },
    {
        name: "ORCID",
        href: "https://orcid.org/0009-0004-1266-8566",
        linkTitle: `Greg Bacon on ORCID`,
        isActive: true,
    },
    {
        name: "LinkedIn",
        href: "https://www.linkedin.com/in/gregbacon/",
        linkTitle: `Greg Bacon on LinkedIn`,
        isActive: true,
    },
];

export const SOCIAL_ICONS: Record<string, string> = {
    Github: "Github",
    Mail: "Mail",
    Linkedin: "LinkedIn",
    "Google Scholar": "GoogleScholar",
    ORCID: "ORCID",
    RSS: "RSS",
};