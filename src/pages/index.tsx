import fs from 'fs/promises';
import { GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import * as path from 'path';
import remarkParse from 'remark-parse';
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
    title: string;
};

const Page: NextPage<Props> = ({ posts }) => (
    <Layout>
        <ul className="list-disc list-inside">
            {posts.map(({ date: [year, month, day], slug, title }) => (
                <li key={slug}>
                    <Link href={`/posts/${year}/${month}/${day}/${slug}`}>
                        <a className="text-sky-800 hover:text-sky-700 dark:text-amber-500 dark:hover:text-amber-400">
                            {title}
                        </a>
                    </Link>{' '}
                    ({year}-{month}-{day})
                </li>
            ))}
        </ul>
    </Layout>
);

const getStaticProps: GetStaticProps<Props> = async () => {
    const paths = await PostRepository.list();

    const posts = [];
    const feed = new RSSFeed();

    for (const path of paths.reverse()) {
        const { body, date, url } = await PostRepository.getByPath(path);
        const mdast = unified().use(remarkParse).parse(body);

        const title = Post.Title.extract(mdast);
        const preface = Post.Preface.extract(mdast);

        posts.push({
            slug: path[3],
            title: Post.Title.extract(mdast),
            date,
        });

        feed.register({ title, preface, date, url });
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
