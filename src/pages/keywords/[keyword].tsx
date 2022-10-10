import { GetStaticPaths, GetStaticProps, NextPage } from 'next';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { LatestPosts, Layout, Links, PostCollection, SideBySide } from '../../components';
import * as Post from '../../Post';
import { PostRepository } from '../../PostRepository';

type Props = {
    keyword: string;
    latestPosts: LatestPost[];
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

const Page: NextPage<Props> = ({ keyword, latestPosts, posts }) => (
    <Layout title={keyword}>
        <div className="px-2 sm:px-4 md:px-6 lg:px-8 pt-4">
            <SideBySide>
                <PostCollection posts={posts} />
                <>
                    <LatestPosts about={keyword} posts={latestPosts} />
                    <Links />
                </>
            </SideBySide>
        </div>
    </Layout>
);

const getStaticPaths: GetStaticPaths = async () => {
    const keys = await PostRepository.list();
    const keywords = new Set<string>();

    for (const path of keys) {
        const post = await PostRepository.lookup(path);

        post.keywords.forEach((keyword) => {
            keywords.add(keyword);
        });
    }

    return {
        fallback: false,
        paths: Array.from(keywords).map((keyword) => ({
            params: {
                keyword,
            },
        })),
    };
};

const getStaticProps: GetStaticProps<Props> = async (ctx) => {
    const keys = await PostRepository.list();

    const posts = [];

    const keyword = ctx.params?.keyword;
    if (typeof keyword !== 'string') {
        return { notFound: true };
    }

    for (const key of keys.reverse()) {
        const { date, keywords, path, preface, preview, slug, title } = await PostRepository.lookup(key);

        if (!keywords.includes(keyword)) {
            continue;
        }

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
    }

    const latestPosts = posts.slice(0, 5);

    return {
        props: {
            keyword,
            latestPosts,
            posts,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
