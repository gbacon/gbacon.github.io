import type { SiteConfig, ThemeConfig, SettingsConfig, UmamiAnalyticsConfig, AnalyticsConfig } from "../types";

export const SITE: SiteConfig = {
    website: "https://blog.gbacon.com/",
    author: "Greg Bacon",
    desc: "Personal site for Greg Bacon.",
    title: "Greg Bacon",
    ogImage: "greg.webp",
    postPerPage: 5,
    favicon: "/favicon.svg",
    lang: "en",
};

export const THEME_CONFIG: ThemeConfig = {
    lightAndDark: true,
    themeLight: "light_default",
    themeDark: "dark_notepad",
};

export const SETTINGS: SettingsConfig = {
    showTagsInNavbar: true,
    showRSSInFooter: true,
    addDevToolsInProduction: true,
};

const umami: UmamiAnalyticsConfig = {
    websiteId: "", // e.g., 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
    // src: "https://cloud.umami.is/script.js", // Default Umami cloud script URL
    src: "",
}

export const ANALYTICS: AnalyticsConfig = {
    // Google Analytics 4 Measurement ID (e.g., 'G-XXXXXXXXXX')
    ga4Id: "G-EVV3E78G51",
    // Umami Analytics configuration
    umami: umami
};
