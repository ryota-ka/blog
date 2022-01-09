module.exports = {
    content: ['./src/pages/**/*.tsx', './src/components/**/*.tsx', './src/custom-elements/**/*.ts'],
    theme: {
        extend: {},
    },
    plugins: [require('@tailwindcss/line-clamp')],
};
