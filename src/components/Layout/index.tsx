import Head from 'next/head';
import Link from 'next/link';
import { useRouter } from 'next/router';

type Props = {
    article?: Article | undefined;
    children: React.ReactNode;
    description?: string | undefined;
    preview?: string | undefined;
    title?: string | undefined;
};

type Article = {
    date: string;
};

export const Layout: React.FC<Props> = ({ article, children, description, preview, title }) => {
    const router = useRouter();

    return (
        <>
            <Head>
                <title>{title === undefined ? 'blog.ryota-ka.me' : `${title} - blog.ryota-ka.me`}</title>
                <link
                    rel="alternate"
                    type="application/rss+xml"
                    title="RSS feed"
                    href="https://blog.ryota-ka.me/feed.xml"
                />
                <meta name="viewport" content="initial-scale=1,width=device-width" />
                {article !== undefined && <meta property="article:published_time" content={article.date} />}
                {description !== undefined && <meta property="og:description" content={description} />}
                <meta property="og:site_name" content="blog.ryota-ka.me" />
                <meta property="og:title" content={title ?? 'blog.ryota-ka.me'} />
                <meta property="og:type" content={article === undefined ? 'website' : 'article'} />
                <meta property="og:url" content={`https://blog.ryota-ka.me${router.asPath}`} />
                {preview !== undefined && <meta property="og:image" content={preview} />}
                <meta name="twitter:card" content="summary_large_image" />
                <meta name="twitter:site" content="@ryotakameoka" />
                <meta name="twitter:title" content={title} />
                {description !== undefined && <meta name="twitter:description" content={description} />}
                {preview !== undefined && <meta name="twitter:image" content={preview} />}
            </Head>
            <header className="px-2 sm:px-4 md:px-6 lg:px-8 py-4 bg-white/50 dark:bg-zinc-900/50 sticky top-0 z-10 backdrop-blur-xs">
                <h1 className="text-base font-medium">
                    <Link href="/">blog.ryota-ka.me</Link>
                </h1>
            </header>
            <main className="relative">{children}</main>
        </>
    );
};
