module.exports = {
    content: ['./src/pages/**/*.tsx', './src/components/**/*.tsx', './src/custom-elements/**/*.ts'],
    theme: {
        extend: {
            brightness: {
                25: '.25',
            },
        },
    },
    plugins: [require('@tailwindcss/line-clamp')],
};
