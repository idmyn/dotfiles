import solarized from "https://raw.githubusercontent.com/zed-industries/zed/main/assets/themes/solarized/solarized.json" with { type: "json" };
import { colord } from "https://esm.sh/colord";

// maybe use Color.js for oklch support
// actually maybe LCH in colord is fine
// https://lch.oklch.com/

const solarizedLight = solarized.themes.find((theme) =>
  theme.appearance === "light"
);

const colors = {
  bg: "#FFFFF8",
  text: "#111111",
  green: "#91EE92",
  purple: "#484FD5",
  ghostWhite: "#E1E7DF", // TODO improve
};

const darken = (color: string) => colord(color).darken(0.25).toHex();

const theme = {
  $schema: "https://zed.dev/schema/themes/v0.1.0.json",
  name: "eink",
  author: "hello@davidmyno.rs",
  themes: [
    {
      name: "eink",
      appearance: "light",
      style: {
        ...solarizedLight.style,
        text: colors.text,
        background: colors.bg,
        "toolbar.background": colors.bg,
        "editor.background": colors.bg,
        "editor.gutter.background": colors.bg,
        "panel.background": colors.bg,
        "search.match_background": colors.green,
        //"element.active": darken(colors.bg),
        //"element.selected": darken(colors.bg),
        "ghost_element.active": colors.ghostWhite,
        "ghost_element.selected": colors.ghostWhite,
        syntax: {
          comment: {
            color: colors.text,
            font_weight: 700,
          },
          keyword: {
            color: colors.purple,
          },
        },
      },
    },
  ],
};

Deno.writeTextFileSync(
  "./eink.json",
  "// machine-generated from ./eink.ts\n// largely based on Zed's solarized theme\n" +
    JSON.stringify(theme, null, 2),
);
