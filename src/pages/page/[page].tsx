import fs from 'fs/promises';
import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import * as path from 'path';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { LatestPosts, Layout, Links, PostCollection, Sidebar, SidebarContent, SideBySide } from '../../components';
import * as Post from '../../Post';
import { PostRepository } from '../../PostRepository';
import { RSSFeed } from '../../RSSFeed';

type Props = {
    keywords: Array<[string, number]>;
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
    slug: string;
    path: string;
    preview: string | null;
    preface: string;
    title: string;
};

const Page: NextPage<Props> = ({ keywords, latestPosts, page, posts }) => (
    <Layout>
        <div className="sm:px-2 md:px-4 pt-4">
            <SideBySide>
                <PostCollection
                    posts={posts}
                    accessory={
                        <Link href={`/page/${page + 1}`}>
                            <a className="block w-72 text-center mx-auto border border-zinc-500 dark:border-zinc-400 py-2">
                                次のページ
                            </a>
                        </Link>
                    }
                />
                <Sidebar>
                    <LatestPosts posts={latestPosts} />
                    <SidebarContent title="Keywords">
                        <ul className="space-y-1 list-disc list-inside pl-2">
                            {keywords.map(([kw, count]) => (
                                <li key={kw}>
                                    <Link href={`/keywords/${kw}`}>
                                        <a className="hover:text-sky-700 dark:hover:text-amber-500">{kw}</a>
                                    </Link>{' '}
                                    ({count})
                                </li>
                            ))}
                        </ul>
                    </SidebarContent>
                    <Links />
                </Sidebar>
            </SideBySide>
        </div>
    </Layout>
);

const PER_PAGE = 5;

const getStaticPaths: GetStaticPaths = async () => {
    const keys = await PostRepository.list();
    const n = Math.floor(keys.length / PER_PAGE);

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
        const { date, path, preface, preview, slug, title } = post;

        const prefaceHTML = unified()
            .use(rehypeStringify)
            .stringify(await Post.Body.transform({ type: 'root', children: preface }));

        posts.push({
            slug,
            title,
            date,
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
            latestPosts,
            page,
            posts,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
