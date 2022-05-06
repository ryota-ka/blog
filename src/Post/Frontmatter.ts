import { Root } from 'mdast';
import * as YAML from 'yaml';
import * as z from 'zod';

type Frontmatter = {
    keywords: string[];
};

const schema: z.Schema<Frontmatter> = z.object({
    keywords: z.array(z.string()),
});

const extract = (root: Root): Frontmatter => {
    const frontmatter = root.children[0];

    if (frontmatter?.type !== 'yaml') {
        throw new Error('A YAML frontmatter must be present in the body');
    }

    const doc = YAML.parse(frontmatter.value) as unknown;

    return schema.parse(doc);
};

export { extract };
export type { Frontmatter };
