/**
 * @type {import('eslint').Linter.Config}
 */
module.exports = {
    env: {
        es2021: true,
    },
    extends: [
        '@herp-inc',
        'plugin:@next/next/recommended',
        'plugin:react-hooks/recommended',
        'plugin:react/recommended',
    ],
    parser: '@typescript-eslint/parser',
    parserOptions: {
        project: './tsconfig.json',
        sourceType: 'module',
    },
    plugins: ['@typescript-eslint', 'import', 'react', 'react-hooks'],
    overrides: [
        // JSX
        {
            files: ['*.tsx'],
            extends: ['plugin:jsx-a11y/recommended'],
            plugins: ['jsx-a11y'],
            rules: {},
        },
        // entrypoint
        {
            files: ['./src/pages/_app.tsx'],
            rules: {
                'import/no-unassigned-import': 'off',
            },
        },
        // pages
        {
            files: ['./src/pages/**/*'],
            rules: {
                'import/no-default-export': 'off',
            },
        },
    ],
    rules: {
        'import/exports-last': 'error',
        'import/group-exports': 'error',
        'jsx-a11y/anchor-is-valid': [
            'error',
            {
                components: ['Link'],
                specialLink: ['hrefLeft', 'hrefRight'],
                aspects: ['invalidHref', 'preferButton'],
            },
        ],
        'react/prop-types': 'off',
        'react/react-in-jsx-scope': 'off',
    },
};
