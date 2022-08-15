import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps } from 'next';
import Image from 'next/image';
import path from 'path';
import rehypeStringify from 'rehype-stringify';
import { unified } from 'unified';

import { Keywords, Layout, SideBySide, TableOfContents } from '../../../../../components';
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
    return (
        <Layout
            article={{ date }}
            title={title}
            description={preface}
            preview={preview === null ? undefined : 'https://blog.ryota-ka.me' + preview}
        >
            <header
                className={
                    `w-full h-36 sm:h-40 md:h-48 mb-2 lg:mb-4 lg:mb-12 relative flex flex-col items-center justify-center text-white bg-zinc-900 ` +
                    (preview === null ? '' : 'lg:h-80')
                }
            >
                {preview !== null && (
                    <Image className="brightness-25" src={preview} layout="fill" objectFit="cover" priority />
                )}
                <h1 className="text-xl lg:text-3xl font-medium w-5/6 text-center z-10">{title}</h1>
                <time className="z-10 mt-2 lg:mt-4 text-base lg:text-xl">{date}</time>
            </header>
            <SideBySide>
                <article
                    className="global-article px-2 sm:px-3 md:px-4 pt-2"
                    dangerouslySetInnerHTML={{ __html: html }}
                />
                <>
                    <Keywords keywords={keywords.map((keyword) => ({ keyword, count: null }))} />
                    {sections.length > 0 && <TableOfContents className="sticky top-8" sections={sections} />}
                </>
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

    const { body, keywords, preface, preview, title } = await PostRepository.lookup([year, month, day, slug]);

    const html = unified()
        .use(rehypeStringify)
        .stringify(await Post.Body.transform(body));

    const tableOfContents = Post.TableOfContents.extract(body);

    return {
        props: {
            date: `${year}-${month}-${day}`,
            keywords,
            html,
            preface: toString({ type: 'root', children: preface }),
            preview,
            sections: tableOfContents,
            title,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
