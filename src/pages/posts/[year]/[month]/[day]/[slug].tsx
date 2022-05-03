import { GetStaticPaths, GetStaticProps } from 'next';
import Image from 'next/image';
import { useRouter } from 'next/router';
import path from 'path';
import remarkParse from 'remark-parse';
import { unified } from 'unified';

import { Layout, TableOfContents } from '../../../../../components';
import * as Post from '../../../../../Post';
import { PostRepository } from '../../../../../PostRepository';

type Props = {
    date: string;
    html: string;
    preface: string;
    preview: string | null;
    sections: Post.TableOfContents.T;
    title: string;
};

const Page: React.FC<Props> = ({ date, html, preface, preview, sections, title }) => {
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
            <div className="flex justify-center flex-wrap sm:px-2 md:px-4 pt-4">
                <article
                    className="global-article w-full lg:w-3/4 max-w-screen-lg"
                    dangerouslySetInnerHTML={{ __html: html }}
                />
                <aside className="hidden shrink-0 lg:block w-1/4 pl-4 max-w-sm">
                    <TableOfContents className="sticky top-4" sections={sections} />
                </aside>
            </div>
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

    const html = await Post.Body.process(body);

    const mdast = unified().use(remarkParse).parse(body);

    const title = Post.Title.extract(mdast);
    const preface = Post.Preface.extract(mdast);

    const tableOfContents = Post.TableOfContents.extract(mdast);

    return {
        props: {
            date: `${year}-${month}-${day}`,
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
