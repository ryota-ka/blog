import { Feed } from 'feed';

type Post = {
    title: string;
    preface: string;
    date: [year: string, month: string, day: string];
    url: string;
};

class RSSFeed {
    #feed: Feed;

    public constructor() {
        this.#feed = new Feed({
            title: 'blog.ryota-ka.me',
            link: 'https://blog.ryota-ka.me/',
            id: 'https://blog.ryota-ka.me/',
            copyright: `Copyright (c) ${new Date().getFullYear()} Ryota Kameoka`,
            author: {
                name: 'Ryota Kameoka',
                email: 'ok@ryota-ka.me',
                link: 'https://ryota-ka.me/',
            },
        });
    }

    public generate(): string {
        return this.#feed.rss2();
    }

    public register({ title, preface, date, url }: Post): void {
        this.#feed.addItem({
            date: new Date(new Date(date.join('-')).valueOf() - 32_400_000),
            description: preface,
            id: url,
            link: url,
            title,
        });
    }
}

export { RSSFeed };
