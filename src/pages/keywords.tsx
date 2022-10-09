import { GetStaticProps, NextPage } from 'next';
import Link from 'next/link';

import { Keywords, LatestPosts, Layout, Links, SideBySide } from '../components';
import { PostRepository } from '../PostRepository';

type LatestPost = {
    title: string;
    path: string;
};

type Props = {
    keywords: Array<[string, number]>;
    latestPosts: LatestPost[];
};

const Page: NextPage<Props> = ({ keywords, latestPosts }) => (
    <Layout title="Keywords">
        <div className="px-2 sm:px-4 md:px-6 lg:px-8 pt-4">
            <SideBySide>
                <>
                    <h1 className="text-2xl mb-4">Keywords</h1>
                    <ul className="space-y-1 text-lg list-['-_'] list-inside marker:text-gray-500 dark:marker:text-gray-400">
                        {keywords.map(([keyword, count]) => (
                            <li key={keyword}>
                                <Link href={`/keywords/${keyword}`}>
                                    <a className="hover:text-sky-700 dark:hover:text-amber-500">{keyword}</a>
                                </Link>{' '}
                                ({count})
                            </li>
                        ))}
                    </ul>
                </>
                <>
                    <LatestPosts posts={latestPosts} />
                    <Keywords keywords={keywords.slice(0, 5).map(([keyword, count]) => ({ keyword, count }))} />
                    <Links />
                </>
            </SideBySide>
        </div>
    </Layout>
);

const getStaticProps: GetStaticProps<Props> = async () => {
    const keys = await PostRepository.list();

    const posts = [];

    for (const key of keys.reverse()) {
        const post = await PostRepository.lookup(key);

        posts.push(post);
    }

    const latestPosts = posts.slice(0, 5);

    const keywords = new Map<string, number>();
    for (const post of posts) {
        for (const kw of post.keywords) {
            const count = keywords.get(kw);

            if (count === undefined) {
                keywords.set(kw, 1);
            } else {
                keywords.set(kw, count + 1);
            }
        }
    }

    return {
        props: {
            keywords: Array.from(keywords.entries()).sort((a, b) =>
                a[1] === b[1] ? a[0].toLowerCase().localeCompare(b[0].toLowerCase()) : a[1] < b[1] ? 1 : -1,
            ),
            latestPosts,
            posts,
        },
    };
};

export default Page;
export { getStaticProps };
