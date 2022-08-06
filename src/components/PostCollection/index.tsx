import Link from 'next/link';

declare namespace PostCollection {
    type Props = {
        accessory?: React.ReactNode;
        posts: Post[];
    };

    type Post = {
        date: [day: string, month: string, day: string];
        slug: string;
        path: string;
        preface: string;
        title: string;
    };
}

const PostCollection: React.FC<PostCollection.Props> = ({ accessory, posts }) => {
    return (
        <section className="space-y-12 md:space-y-24 px-2 sm:px-3 md:px-4">
            {posts.map(({ date: [year, month, day], slug, preface, path, title }) => (
                <article key={slug}>
                    <header className="w-full mb-2 lg:mb-4">
                        <time className="block md:mb-2 text-base lg:text-xl">
                            {year}-{month}-{day}
                        </time>
                        <h1 className="text-xl lg:text-3xl font-medium leading-relaxed pb-2">
                            <Link href={path}>
                                <a>{title}</a>
                            </Link>
                        </h1>
                    </header>
                    <div>
                        <div className="global-article" dangerouslySetInnerHTML={{ __html: preface }} />
                        <Link href={path + '#more'}>
                            <a className="block w-48 mt-6 mx-auto border border-zinc-500 dark:border-zinc-600 px-4 py-2 text-center dark:text-white">
                                続きを読む
                            </a>
                        </Link>
                    </div>
                </article>
            ))}
            {accessory}
        </section>
    );
};

export { PostCollection };
