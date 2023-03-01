import type * as hast from 'hast';
import groovy from 'highlight.js/lib/languages/groovy';
import haskell from 'highlight.js/lib/languages/haskell';
import nix from 'highlight.js/lib/languages/nix';
import vim from 'highlight.js/lib/languages/vim';
import type * as mdast from 'mdast';
import { defaultHandlers } from 'mdast-util-to-hast/lib';
import type { H, MdastNode } from 'mdast-util-to-hast/lib';
import { toString } from 'mdast-util-to-string';
import rehypeAutolinkHeadings from 'rehype-autolink-headings';
import rehypeHighlight from 'rehype-highlight';
import rehypeKatex from 'rehype-katex';
import rehypeStringify from 'rehype-stringify';
import remarkFrontmatter from 'remark-frontmatter';
import remarkGfm from 'remark-gfm';
import remarkMath from 'remark-math';
import remarkParse from 'remark-parse';
import remarkRehype, { all } from 'remark-rehype';
import { unified } from 'unified';
import { u } from 'unist-builder';

import { fancyLinks, removeTitle, sectionalize, slugger, targetBlank } from '../unified-plugins';

const parser = unified().use(remarkParse).use(remarkGfm).use(remarkMath).use(remarkFrontmatter, ['yaml']).freeze();

const transformer = unified()
    .use(removeTitle)
    .use(fancyLinks)
    .use(targetBlank)
    .use(remarkRehype, {
        handlers: {
            code: (h: H, node: MdastNode) => {
                if (node.type !== 'code') {
                    return;
                }

                if (node.lang === 'mermaid') {
                    return h(node, 'div', { class: 'mermaid' }, [u('text', node.value)]);
                }

                const meta = new Map(
                    (node.meta?.split(' ') ?? []).map((x) => x.split('=').slice(0, 2) as [string, string]),
                );
                const filename = meta.get('filename');

                const hastNode = defaultHandlers.code(h, node);

                if (filename === undefined || hastNode === null || hastNode === undefined) {
                    return hastNode;
                }

                return h(node, 'div', { class: 'codeblock-with-metadata' }, [
                    h(node, 'div', { class: 'codeblock-with-metadata__header' }, [
                        h(node, 'pre', { class: 'codeblock-with-metadata__filename' }, [u('text', filename)]),
                    ]),
                    ...[hastNode].flat(),
                ]);
            },
            footnoteDefinition: (h: H, node: MdastNode) => {
                if (node.type !== 'footnoteDefinition') {
                    return;
                }

                const child = node.children[0];
                if (node.children.length !== 1 || child?.type !== 'paragraph') {
                    throw new Error(`A footnote definition can contain the single paragraph.`);
                }

                h.footnoteById[node.identifier] = node;

                const id = node.identifier;

                return h(node, 'p', { id: `fn-${id}` }, [
                    h(node, 'a', { href: `#fnref-${id}` }, [u('text', `*${id}`)]),
                    u('text', ': '),
                    ...all(h, child),
                ]);
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
            thematicBreak: (h: H, node: MdastNode) => h(node, 'span', { id: 'more' }, []),
        },
    })
    .use(sectionalize)
    .use(rehypeKatex, { strict: true })
    .use(rehypeHighlight, {
        aliases: {
            perl: ['perl6'],
        },
        ignoreMissing: true,
        languages: {
            groovy,
            haskell,
            nix,
            vim,
        },
    })
    .use(slugger)
    .use(rehypeAutolinkHeadings, {
        behavior: 'append',
        properties: {},
        content: {
            type: 'text',
            value: '#',
        },
    })
    .use(rehypeStringify)
    .freeze();

const parse = (body: string): mdast.Root => parser.parse(body);

const transform = async (node: mdast.Root): Promise<hast.Root> => await transformer.run(node);

const process = async (body: string): Promise<string> => {
    const mdast = parse(body);
    const hast = await transform(mdast);
    return unified().use(rehypeStringify).stringify(hast);
};

export { parse, process, transform };
