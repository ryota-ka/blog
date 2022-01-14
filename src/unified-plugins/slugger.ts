import Slugger from 'github-slugger';
import type { Root } from 'hast';
import { headingRank } from 'hast-util-heading-rank';
import { toString } from 'hast-util-to-string';
import type { Plugin } from 'unified';
import { visit } from 'unist-util-visit';

export const slugger: Plugin<[], Root, Root> = () => {
    const slugs = new Slugger();

    return (tree) => {
        visit(tree, 'element', (node) => {
            const rank = headingRank(node);
            if (rank === null || rank === 1) {
                return;
            }

            const slug = slugs.slug(toString(node));

            node.properties ??= {};
            node.properties.id = slug;
        });
    };
};
