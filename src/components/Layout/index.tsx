import Head from 'next/head';
import Link from 'next/link';
import { useRouter } from 'next/router';

type Props = {
    article?: Article | undefined;
    description?: string | undefined;
    title?: string | undefined;
};

type Article = {
    date: string;
};

export const Layout: React.FC<Props> = ({ article, children, description, title }) => {
    const router = useRouter();

    return (
        <>
            <Head>
                <title>{title === undefined ? 'blog.ryota-ka.me' : `${title} - blog.ryota-ka.me`}</title>
                <meta name="viewport" content="initial-scale=1,width=device-width" />
                {article !== undefined && <meta property="article:published_time" content={article.date} />}
                {description !== undefined && <meta property="og:description" content={description} />}
                <meta property="og:site_name" content="blog.ryota-ka.me" />
                <meta property="og:title" content={title ?? 'blog.ryota-ka.me'} />
                <meta property="og:type" content={article === undefined ? 'website' : 'article'} />
                <meta property="og:url" content={`https://blog.ryota-ka.me${router.asPath}`} />
            </Head>
            <header className="px-16 py-4 bg-white drop-shadow">
                <h1 className="text-base">
                    <Link href="/">blog.ryota-ka.me</Link>
                </h1>
            </header>
            <main className="px-16 pt-4">{children}</main>
        </>
    );
};
