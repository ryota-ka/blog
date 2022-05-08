import fs from 'fs/promises';
import type { Content, Root } from 'mdast';
import path from 'path';
import remarkParse from 'remark-parse';
import { unified } from 'unified';

import * as Post from './Post';

type Key = [year: string, month: string, day: string, slug: string];

type Post = {
    body: Root;
    date: [year: string, month: string, day: string];
    keywords: string[];
    path: string;
    preface: Content[];
    preview: string | null;
    slug: string;
    title: string;
};

const mdRoot = path.join(process.cwd(), 'data', 'posts');
const assetsRoot = path.join(process.cwd(), 'public', 'posts');

const PostRepository = {
    async lookup([year, month, day, slug]: Key): Promise<Post> {
        const md = await fs.readFile(path.join(mdRoot, year, month, day, `${slug}.md`), { encoding: 'utf-8' });

        let previewExists = false;
        try {
            await fs.access(path.join(assetsRoot, year, month, day, slug, 'preview.png'));
            previewExists = true;
        } catch {
            // nop
        }

        const body = Post.Body.parse(md);
        const preface = Post.Preface.extract(body);
        const { keywords } = Post.Frontmatter.extract(body);

        const title = Post.Title.extract(unified().use(remarkParse).parse(md));

        return {
            body,
            date: [year, month, day],
            keywords,
            path: `/posts/${year}/${month}/${day}/${slug}`,
            preface,
            preview: previewExists ? `/posts/${year}/${month}/${day}/${slug}/preview.png` : null,
            slug,
            title,
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
