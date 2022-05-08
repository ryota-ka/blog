import fs from 'fs/promises';
import path from 'path';

type Key = [year: string, month: string, day: string, slug: string];

type Post = {
    body: string;
    date: [year: string, month: string, day: string];
    preview: string | null;
    slug: string;
    url: string;
};

const mdRoot = path.join(process.cwd(), 'data', 'posts');
const assetsRoot = path.join(process.cwd(), 'public', 'posts');

const PostRepository = {
    async lookup([year, month, day, slug]: Key): Promise<Post> {
        const body = await fs.readFile(path.join(mdRoot, year, month, day, `${slug}.md`), { encoding: 'utf-8' });

        let previewExists = false;
        try {
            await fs.access(path.join(assetsRoot, year, month, day, slug, 'preview.png'));
            previewExists = true;
        } catch {
            // nop
        }

        return {
            body,
            date: [year, month, day],
            preview: previewExists
                ? `https://blog.ryota-ka.me/posts/${year}/${month}/${day}/${slug}/preview.png`
                : null,
            slug,
            url: `https://blog.ryota-ka.me/posts/${year}/${month}/${day}/${slug}`,
        };
    },

    async list(): Promise<Key[]> {
        const keys: Key[] = [];

        const years = await fs.readdir(mdRoot);

        for (const year of years) {
            const months = await fs.readdir(path.join(mdRoot, year));

            for (const month of months) {
                const days = await fs.readdir(path.join(mdRoot, year, month));

                for (const day of days) {
                    const files = await fs.readdir(path.join(mdRoot, year, month, day));

                    for (const file of files) {
                        keys.push([year, month, day, path.parse(file).name]);
                    }
                }
            }
        }

        return keys;
    },
};

export { type Key, type Post, PostRepository };
