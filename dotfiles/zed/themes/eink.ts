import solarized from "https://raw.githubusercontent.com/zed-industries/zed/main/assets/themes/solarized/solarized.json" with { type: "json" };

const solarizedLight = solarized.themes.find(theme => theme.appearance === 'light')

const colors = {
  bg: "#fffff8",
  text: "#111111",
  blue: "#278ad1ff",
  green: "#91EE92",
};

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
        // background: colors.bg,
        // "toolbar.background": colors.bg,
        // "editor.background": colors.bg,
        // text: colors.text,
        // "editor.gutter.background": colors.bg,
        // "panel.background": colors.bg,
        // "search.match_background": colors.green,
        syntax: {
          comment: {
            color: colors.text,
            font_weight: 700,
          },
          keyword: {
            color: colors.blue,
          },
        },
      },
    },
  ],
};

Deno.writeTextFileSync("./eink.json", "// machine-generated from ./eink.ts\n// largely based on Zed's solarized theme\n" + JSON.stringify(theme, null, 2));
