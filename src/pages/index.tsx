import fs from 'fs/promises';
import { toString } from 'mdast-util-to-string';
import { GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import * as path from 'path';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { Layout } from '../components';
import * as Post from '../Post';
import { PostRepository } from '../PostRepository';
import { RSSFeed } from '../RSSFeed';

type Props = {
    posts: Post[];
};

type Post = {
    date: [day: string, month: string, day: string];
    slug: string;
    preface: string;
    title: string;
};

const Page: NextPage<Props> = ({ posts }) => (
    <Layout>
        <div className="sm:px-2 md:px-4 pt-4">
            <div className="flex justify-center flex-wrap sm:px-2 md:px-4 pt-4">
                <div className="w-full lg:w-3/4 max-w-screen-lg space-y-6 md:space-y-8">
                    {posts.map(({ date: [year, month, day], slug, preface, title }) => (
                        <article
                            key={slug}
                            className="sm:rounded-md md:rounded-lg px-2 py-4 sm:p-3 md:p-4 shadow bg-zinc-50 dark:bg-zinc-900"
                        >
                            <time className="block sm:mb-1 text-sm sm:text-base">
                                {year}-{month}-{day}
                            </time>
                            <Link href={`/posts/${year}/${month}/${day}/${slug}`}>
                                <a>
                                    <h1 className="border-b border-zinc-400 mb-3 md:mb-6 pb-2 text-lg sm:text-xl md:text-2xl">
                                        {title}
                                    </h1>
                                </a>
                            </Link>
                            <div className="global-article" dangerouslySetInnerHTML={{ __html: preface }} />
                            <Link href={`/posts/${year}/${month}/${day}/${slug}#more`}>
                                <a className="block w-48 mt-4 md:mt-6 mx-auto border px-4 py-2 text-center dark:text-white">
                                    続きを読む
                                </a>
                            </Link>
                        </article>
                    ))}
                </div>
                <aside className="hidden shrink-0 lg:block w-1/4 pl-4 max-w-sm">
                    <div className="bg-zinc-50 dark:bg-zinc-900 rounded-xl px-2 py-4 shadow">
                        <div className="text-lg mb-2 font-bold px-2">Links</div>
                        <ul className="list-inside list-disc ml-2 space-y-1">
                            <li>
                                <a href="https://ryota-ka.me/" target="_blank" rel="noopener noreferrer">
                                    ryota-ka.me
                                </a>
                            </li>
                            <li>
                                <a href="https://twitter.com/ryotakameoka" target="_blank" rel="noopener noreferrer">
                                    Twitter
                                </a>
                            </li>
                        </ul>
                    </div>
                </aside>
            </div>
        </div>
    </Layout>
);

const getStaticProps: GetStaticProps<Props> = async () => {
    const paths = await PostRepository.list();

    const posts = [];
    const feed = new RSSFeed();

    for (const path of paths.reverse()) {
        const { body, date, url } = await PostRepository.getByPath(path);
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

    await fs.writeFile(path.join(process.cwd(), 'public', 'feed.xml'), feed.generate());

    return {
        props: {
            posts,
        },
    };
};

export default Page;
export { getStaticProps };
