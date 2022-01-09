import type { FootnoteDefinition, ListItem, Root } from 'mdast';
import type { Plugin } from 'unified';
import { u } from 'unist-builder';

const organizeFootnotes: Plugin<[{ label: string }], Root, Root> =
    ({ label }) =>
    (tree: Root) => {
        const index = tree.children.findIndex((node) => node.type === 'footnoteDefinition');

        tree.children = [
            ...tree.children.slice(0, index),
            {
                type: 'heading',
                depth: 2,
                children: [u('text', label)],
                data: {
                    hProperties: { id: 'footnotes' },
                },
            },
            {
                type: 'list',
                ordered: true,
                children: tree.children
                    .slice(index)
                    .filter((node): node is FootnoteDefinition => node.type === 'footnoteDefinition')
                    .map<ListItem>((dfn) => ({
                        type: 'listItem',
                        children: dfn.children,
                        position: dfn.position,
                        data: {
                            hProperties: {
                                name: `fn-${dfn.identifier}`,
                                id: `fn-${dfn.identifier}`,
                            },
                        },
                    })),
            },
        ];
    };

export { organizeFootnotes };
