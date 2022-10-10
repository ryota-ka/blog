import fs from 'fs/promises';
import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import * as path from 'path';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { Keywords, LatestPosts, Layout, Links, PostCollection, SideBySide } from '../../components';
import * as Post from '../../Post';
import { PostRepository } from '../../PostRepository';
import { RSSFeed } from '../../RSSFeed';

type Props = {
    keywords: Array<[string, number]>;
    hasMore: boolean;
    latestPosts: LatestPost[];
    page: number;
    posts: Post[];
};

type LatestPost = {
    title: string;
    path: string;
};

type Post = {
    date: [day: string, month: string, day: string];
    keywords: string[];
    slug: string;
    path: string;
    preview: string | null;
    preface: string;
    title: string;
};

const Page: NextPage<Props> = ({ keywords, hasMore, latestPosts, page, posts }) => (
    <Layout>
        <div className="px-2 sm:px-4 md:px-6 lg:px-8 pt-4">
            <SideBySide>
                <PostCollection
                    posts={posts}
                    accessory={
                        hasMore && (
                            <Link href={`/page/${page + 1}`}>
                                <a className="block w-72 text-center mx-auto border border-zinc-500 dark:border-zinc-400 py-2">
                                    次のページ
                                </a>
                            </Link>
                        )
                    }
                />
                <>
                    <LatestPosts posts={latestPosts} />
                    <Keywords
                        keywords={keywords.map(([keyword, count]) => ({ keyword, count }))}
                        seeAllKeywords={true}
                    />
                    <Links />
                </>
            </SideBySide>
        </div>
    </Layout>
);

const PER_PAGE = 5;

const getStaticPaths: GetStaticPaths = async () => {
    const keys = await PostRepository.list();
    const n = Math.ceil(keys.length / PER_PAGE);

    return {
        fallback: false,
        paths: Array.from({ length: n }).map((_, i) => ({
            params: {
                page: String(i + 1),
            },
        })),
    };
};

const getStaticProps: GetStaticProps<Props> = async (ctx) => {
    const keys = await PostRepository.list();

    const posts = [];
    const feed = new RSSFeed();

    const pagestr = ctx.params?.page ?? '1';
    if (typeof pagestr !== 'string') {
        return { notFound: true };
    }
    const page = Number.parseInt(pagestr, 10);
    const offset = (page - 1) * PER_PAGE;

    const ps = await Promise.all(keys.reverse().map(async (key) => await PostRepository.lookup(key)));

    for (const post of ps.slice(offset, offset + PER_PAGE)) {
        const { date, keywords, path, preface, preview, slug, title } = post;

        const prefaceHTML = unified()
            .use(rehypeStringify)
            .stringify(await Post.Body.transform({ type: 'root', children: preface }));

        posts.push({
            slug,
            title,
            date,
            keywords,
            path,
            preview,
            preface: prefaceHTML,
        });

        feed.register({
            title,
            preface: toString({
                type: 'root',
                children: preface,
            }),
            date,
            url: 'https://blog.ryota-ka.me' + path,
        });
    }

    const keywords = new Map<string, number>();
    for (const post of ps) {
        for (const kw of post.keywords) {
            const count = keywords.get(kw);

            if (count === undefined) {
                keywords.set(kw, 1);
            } else {
                keywords.set(kw, count + 1);
            }
        }
    }

    if (page === 1) {
        await fs.writeFile(path.join(process.cwd(), 'public', 'feed.xml'), feed.generate());
    }

    const latestPosts = ps.slice(0, 5);

    return {
        props: {
            keywords: Array.from(keywords.entries())
                .sort((a, b) =>
                    a[1] === b[1] ? a[0].toLowerCase().localeCompare(b[0].toLowerCase()) : a[1] < b[1] ? 1 : -1,
                )
                .slice(0, 5),
            hasMore: ps.length > offset + PER_PAGE,
            latestPosts,
            page,
            posts,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
