import type { Element, H, MdastNode } from 'mdast-util-to-hast/lib';
import { toString } from 'mdast-util-to-string';
import { GetStaticPaths, GetStaticProps } from 'next';
import path from 'path';
import rehypeHighlight from 'rehype-highlight';
import rehypeKatex from 'rehype-katex';
import rehypeStringify from 'rehype-stringify';
import remarkGfm from 'remark-gfm';
import remarkMath from 'remark-math';
import remarkParse from 'remark-parse';
import remarkRehype, { all } from 'remark-rehype';
import { unified } from 'unified';
import { u } from 'unist-builder';

import { Layout } from '../../../../../components';
import * as Post from '../../../../../Post';
import { PostRepository } from '../../../../../PostRepository';

type Props = {
    date: string;
    html: string;
    preface: string;
    title: string;
};

const Page: React.FC<Props> = ({ date, html, preface, title }) => {
    return (
        <Layout article={{ date }} title={title} description={preface}>
            <article className="global-article" dangerouslySetInnerHTML={{ __html: html }} />
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

    const { body } = await PostRepository.getByPath([year, month, day, slug]);

    const mdast = unified().use(remarkParse).use(remarkGfm).use(remarkMath).parse(body);

    const hast = await unified()
        .use(remarkRehype, {
            handlers: {
                footnoteDefinition(h: H, node: MdastNode) {
                    if (node.type !== 'footnoteDefinition') {
                        return;
                    }

                    const id = node.identifier;

                    return h(
                        node,
                        'li',
                        { dataFootnoteDefinition: true, name: `fn-${id}`, id: `fn-${id}` },
                        all(h, node),
                    );
                },
                footnoteReference: (h: H, node: MdastNode) => {
                    if (node.type !== 'footnoteReference') {
                        return;
                    }

                    const id = node.identifier;

                    return h(node, 'sup', [
                        h(
                            node.position,
                            'a',
                            {
                                ariaDescribedby: `#fn-${id}`,
                                href: `#fn-${id}`,
                                id: `fnref-${id}`,
                                title: toString(h.footnoteById[id]),
                            },
                            [u('text', `*${node.identifier}`)],
                        ),
                    ]);
                },
            },
        })
        .use(rehypeKatex, { strict: true })
        .use(rehypeHighlight)
        .run(mdast);

    const index = hast.children.findIndex((node) => {
        return node.type === 'element' && node.properties?.dataFootnoteDefinition === true;
    });

    hast.children = [
        ...hast.children.slice(0, index),
        {
            type: 'element',
            tagName: 'section',
            properties: {
                id: 'footnotes',
            },
            children: [
                {
                    type: 'element',
                    tagName: 'h2',
                    children: [u('text', '脚注')],
                },
                {
                    type: 'element',
                    tagName: 'ol',
                    children: hast.children
                        .slice(index)
                        .filter<Element>((node): node is Element => node.type === 'element'),
                },
            ],
        },
    ];

    const html = unified().use(rehypeStringify).stringify(hast);

    const title = Post.Title.extract(mdast);
    const preface = Post.Preface.extract(mdast);

    return {
        props: {
            date: `${year}-${month}-${day}`,
            html,
            preface,
            title,
        },
    };
};

export default Page;
export { getStaticPaths, getStaticProps };
