import fs from 'fs/promises';
import path from 'path';

type Path = [year: string, month: string, day: string, slug: string];

type Post = {
    body: string;
    date: [year: string, month: string, day: string];
};

const root = path.join(process.cwd(), 'data', 'posts');

const PostRepository = {
    async getByPath([year, month, day, slug]: Path): Promise<Post> {
        const body = await fs.readFile(path.join(root, year, month, day, `${slug}.md`), { encoding: 'utf-8' });

        return {
            body,
            date: [year, month, day],
        };
    },

    async list(): Promise<Path[]> {
        const paths: Path[] = [];

        const years = await fs.readdir(root);

        for (const year of years) {
            const months = await fs.readdir(path.join(root, year));

            for (const month of months) {
                const days = await fs.readdir(path.join(root, year, month));

                for (const day of days) {
                    const files = await fs.readdir(path.join(root, year, month, day));

                    for (const file of files) {
                        paths.push([year, month, day, path.parse(file).name]);
                    }
                }
            }
        }

        return paths;
    },
};

export { type Path, type Post, PostRepository };
