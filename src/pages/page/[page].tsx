import fs from 'fs/promises';
import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import * as path from 'path';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { Layout, Links, PostCollection, SideBySide } from '../../components';
import * as Post from '../../Post';
import { PostRepository } from '../../PostRepository';
import { RSSFeed } from '../../RSSFeed';

type Props = {
    page: number;
    posts: Post[];
};

type Post = {
    date: [day: string, month: string, day: string];
    slug: string;
    preview: string | null;
    preface: string;
    title: string;
};

const Page: NextPage<Props> = ({ page, posts }) => (
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
                <Links />
            </SideBySide>
        </div>
    </Layout>
);

const PER_PAGE = 5;

const getStaticPaths: GetStaticPaths = async () => {
    const paths = await PostRepository.list();
    const n = Math.floor(paths.length / PER_PAGE);

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
    const paths = await PostRepository.list();

    const posts = [];
    const feed = new RSSFeed();

    const pagestr = ctx.params?.page ?? '1';
    if (typeof pagestr !== 'string') {
        return { notFound: true };
    }
    const page = Number.parseInt(pagestr, 10);
    const offset = (page - 1) * PER_PAGE;

    for (const path of paths.reverse().slice(offset, offset + PER_PAGE)) {
        const { body, date, preview, url } = await PostRepository.lookup(path);
        const mdast = Post.Body.parse(body);

        const title = Post.Title.extract(mdast);
        const preface = Post.Preface.extract(mdast);

        const prefaceHTML = unified()
            .use(rehypeStringify)
            .stringify(await Post.Body.transform({ type: 'root', children: preface }));

        posts.push({
            slug: path[3],
            title: Post.Title.extract(mdast),
            date,
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
            url,
        });
    }

    if (page === 1) {
        await fs.writeFile(path.join(process.cwd(), 'public', 'feed.xml'), feed.generate());
    }

    return {
        props: {
            page,
            posts,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
