import Head from 'next/head';
import Link from 'next/link';
import { useRouter } from 'next/router';

type Props = {
    article?: Article | undefined;
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
            <header className="px-2 sm:px-4 md:px-6 lg:px-8 py-4 bg-white dark:bg-slate-800 drop-shadow">
                <h1 className="text-base">
                    <Link href="/">blog.ryota-ka.me</Link>
                </h1>
            </header>
            <main className="px-2 sm:px-4 md:px-6 lg:px-8 pt-4">{children}</main>
        </>
    );
};
