/**
 * @type {import('tailwindcss').Config}
 */
module.exports = {
    content: ['./src/pages/**/*.tsx', './src/components/**/*.tsx', './src/custom-elements/**/*.ts'],
    theme: {
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
