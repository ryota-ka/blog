import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps } from 'next';
import Image from 'next/image';
import { useRouter } from 'next/router';
import path from 'path';
import rehypeStringify from 'rehype-stringify';
import remarkParse from 'remark-parse';
import { unified } from 'unified';

import { Keywords, Layout, Sidebar, SideBySide, TableOfContents } from '../../../../../components';
import * as Post from '../../../../../Post';
import { PostRepository } from '../../../../../PostRepository';

type Props = {
    date: string;
    keywords: string[];
    html: string;
    preface: string;
    preview: string | null;
    sections: Post.TableOfContents.T;
    title: string;
};

const Page: React.FC<Props> = ({ date, html, keywords, preface, preview, sections, title }) => {
    const router = useRouter();

    return (
        <Layout article={{ date }} title={title} description={preface} preview={preview ?? undefined}>
            <header
                className={
                    `w-full h-48 mb-2 lg:mb-4 lg:mb-12 relative flex flex-col items-center justify-center text-white bg-zinc-900 ` +
                    (preview === null ? '' : 'lg:h-80')
                }
            >
                {preview !== null && (
                    <Image
                        className="brightness-25"
                        src={new URL(router.asPath, 'https://example.com').pathname + '/preview.png'}
                        layout="fill"
                        objectFit="cover"
                    />
                )}
                <h1 className="text-xl lg:text-3xl font-semibold w-5/6 lg:w-2/3 text-center z-10">{title}</h1>
                <time className="z-10 mt-2 lg:mt-4 text-base lg:text-xl">{date}</time>
            </header>
            <SideBySide>
                <article
                    className="global-article sm:rounded-xl p-2 sm:p-3 md:p-4 shadow bg-zinc-50 dark:bg-zinc-900 dark:border-y sm:dark:border dark:border-zinc-700"
                    dangerouslySetInnerHTML={{ __html: html }}
                />
                <Sidebar className="sticky top-4">
                    <Keywords keywords={keywords} />
                    <TableOfContents sections={sections} />
                </Sidebar>
            </SideBySide>
        </Layout>
    );
};

const getStaticPaths: GetStaticPaths = async () => {
    const posts = await PostRepository.list();

    const paths = posts.map(([year, month, day, filename]) => ({
        params: {
            year,
            month,
            day,
            slug: path.parse(filename).name,
        },
    }));

    return {
        paths,
        fallback: false,
    };
};

const getStaticProps: GetStaticProps<Props> = async (ctx) => {
    const year = ctx.params?.year;
    const month = ctx.params?.month;
    const day = ctx.params?.day;
    const slug = ctx.params?.slug;

    if (
        year === undefined ||
        Array.isArray(year) ||
        month === undefined ||
        Array.isArray(month) ||
        day === undefined ||
        Array.isArray(day) ||
        slug === undefined ||
        Array.isArray(slug)
    ) {
        return {
            notFound: true,
        };
    }

    const { body, preview } = await PostRepository.getByPath([year, month, day, slug]);

    const mdast = Post.Body.parse(body);
    const html = unified()
        .use(rehypeStringify)
        .stringify(await Post.Body.transform(mdast));

    const title = Post.Title.extract(unified().use(remarkParse).parse(body));
    const preface = toString({ type: 'root', children: Post.Preface.extract(mdast) });
    const { keywords } = Post.Frontmatter.extract(mdast);

    const tableOfContents = Post.TableOfContents.extract(mdast);

    return {
        props: {
            date: `${year}-${month}-${day}`,
            keywords,
            html,
            preface,
            preview,
            sections: tableOfContents,
            title,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
