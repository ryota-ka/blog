import { GetStaticProps, NextPage } from 'next';
import Link from 'next/link';
import remarkParse from 'remark-parse';
import { unified } from 'unified';

import { Layout } from '../components';
import * as Post from '../Post';
import { PostRepository } from '../PostRepository';

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
                        <a className="text-sky-800">{title}</a>
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

    for (const path of paths.reverse()) {
        const { body, date } = await PostRepository.getByPath(path);
        const mdast = unified().use(remarkParse).parse(body);

        posts.push({
            slug: path[3],
            title: Post.Title.extract(mdast),
            date,
        });
    }

    return {
        props: {
            posts,
        },
    };
};

export default Page;
export { getStaticProps };
