import Document, { Head, Html, Main, NextScript } from 'next/document';

export default class MyDocument extends Document {
    public render(): React.ReactElement {
        return (
            <Html lang="ja" className="scroll-pt-8 scroll-smooth">
                <Head />
                <body className="bg-zinc-100 dark:bg-black dark:text-zinc-50 pb-12">
                    <Main />
                    <NextScript />
                </body>
            </Html>
        );
    }
}
