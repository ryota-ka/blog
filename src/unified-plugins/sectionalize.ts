import type { ElementContent, Root, RootContent } from 'hast';
import type { Plugin } from 'unified';

const sectionalize: Plugin<[], Root, void> = () => async (tree: Root) => {
    const children: RootContent[] = [];

    let h2SectionContent: ElementContent[] = [];
    let h3SectionContent: ElementContent[] = [];

    const pushH2Section = (): void => {
        if (h2SectionContent.length === 0) {
            return;
        }

        children.push({
            tagName: 'section',
            type: 'element',
            children: h2SectionContent,
        });
        h2SectionContent = [];
    };

    const pushH3Section = (): void => {
        if (h3SectionContent.length === 0) {
            return;
        }

        h2SectionContent.push({
            tagName: 'section',
            type: 'element',
            children: h3SectionContent,
        });
        h3SectionContent = [];
    };

    for (const child of tree.children) {
        if (child.type === 'element') {
            switch (child.tagName) {
                case 'h2':
                    pushH3Section();
                    pushH2Section();
                    break;
                case 'h3':
                    pushH3Section();
                    break;
            }
        }

        if (child.type === 'doctype') {
            children.push(child);
        } else {
            h3SectionContent.push(child);
        }
    }

    pushH3Section();
    pushH2Section();

    return { ...tree, children };
};

export { sectionalize };
