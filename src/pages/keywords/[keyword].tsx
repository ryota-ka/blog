import { GetStaticPaths, GetStaticProps, NextPage } from 'next';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { Layout, Links, PostCollection, SideBySide } from '../../components';
import * as Post from '../../Post';
import { PostRepository } from '../../PostRepository';

type Props = {
    keyword: string;
    posts: Post[];
};

type Post = {
    date: [day: string, month: string, day: string];
    slug: string;
    preview: string | null;
    preface: string;
    title: string;
};

const Page: NextPage<Props> = ({ keyword, posts }) => (
    <Layout title={keyword}>
        <div className="sm:px-2 md:px-4 pt-4">
            <SideBySide>
                <PostCollection posts={posts} />
                <Links />
            </SideBySide>
        </div>
    </Layout>
);

const getStaticPaths: GetStaticPaths = async () => {
    const keys = await PostRepository.list();
    const keywords = new Set<string>();

    for (const path of keys) {
        const { body } = await PostRepository.lookup(path);
        const mdast = Post.Body.parse(body);
        const frontmatter = Post.Frontmatter.extract(mdast);

        frontmatter.keywords.forEach((keyword) => {
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
        const { body, date, preview, slug } = await PostRepository.lookup(key);
        const mdast = Post.Body.parse(body);

        const { keywords } = Post.Frontmatter.extract(mdast);

        if (!keywords.includes(keyword)) {
            continue;
        }

        const preface = Post.Preface.extract(mdast);

        const prefaceHTML = unified()
            .use(rehypeStringify)
            .stringify(await Post.Body.transform({ type: 'root', children: preface }));

        posts.push({
            slug,
            title: Post.Title.extract(mdast),
            date,
            preview,
            preface: prefaceHTML,
        });
    }

    return {
        props: {
            keyword,
            posts,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
