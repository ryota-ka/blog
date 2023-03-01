const defaultTheme = require('tailwindcss/defaultTheme');

/**
 * @type {import('tailwindcss').Config}
 */
module.exports = {
    content: ['./src/pages/**/*.tsx', './src/components/**/*.tsx', './src/custom-elements/**/*.ts'],
    theme: {
        fontFamily: {
            mono: [
                '"JetBrains Mono"',
                '"Monoid"',
                '"Source Code Pro"',
                '"Consolas"',
                '"Inconsolata"',
                ...defaultTheme.fontFamily.mono,
            ],
            sans: [
                defaultTheme.fontFamily.sans,
                {
                    fontFeatureSettings: '"palt"',
                },
            ],
            serif: [
                [
                    '"Times New Roman"',
                    '"YuMincho"',
                    '"Yu Mincho"',
                    '"Hiragino Mincho ProN"',
                    ...defaultTheme.fontFamily.serif,
                ],
                {
                    fontFeatureSettings: '"palt"',
                },
            ],
        },
        extend: {
            backdropBlur: {
                xs: '2px',
            },
            brightness: {
                25: '.25',
            },
        },
    },
    plugins: [require('@tailwindcss/line-clamp')],
};
