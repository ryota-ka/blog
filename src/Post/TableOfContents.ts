import Slugger from 'github-slugger';
import { Root } from 'mdast';
import { toString } from 'mdast-util-to-string';

type T = Section[];

type Section = {
    title: string;
    href: string;
    subsections: Subsection[];
};

type Subsection = {
    title: string;
    href: string;
};

const extract = (root: Root): Section[] => {
    const slugger = new Slugger();

    const toc: Section[] = [];

    for (const child of root.children) {
        if (child.type !== 'heading') {
            continue;
        }

        const title = toString(child);

        const slug = slugger.slug(title);
        const href = '#' + slug;

        if (child.depth === 2) {
            toc.push({
                title,
                href,
                subsections: [],
            });
        } else if (child.depth === 3) {
            const section = toc[toc.length - 1];

            if (section === undefined) {
                throw new Error('A level-3 heading cannot be appear before a level-2 heading.');
            }

            section.subsections.push({
                title,
                href,
            });
        }
    }

    return toc;
};

export { extract, type Section, type Subsection, type T };
