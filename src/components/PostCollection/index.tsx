import Link from 'next/link';

declare namespace PostCollection {
    type Props = {
        accessory?: React.ReactNode;
        posts: Post[];
    };

    type Post = {
        date: [day: string, month: string, day: string];
        keywords: string[];
        slug: string;
        path: string;
        preface: string;
        title: string;
    };
}

const PostCollection: React.FC<PostCollection.Props> = ({ accessory, posts }) => {
    return (
        <section className="space-y-12 md:space-y-24">
            {posts.map(({ date: [year, month, day], keywords, slug, preface, path, title }) => (
                <article key={slug}>
                    <header className="w-full mb-4 md:mb-6 lg:mb-8">
                        <time className="block md:mb-2 text-base lg:text-xl">
                            {year}-{month}-{day}
                        </time>
                        <h1 className="text-xl lg:text-3xl font-medium leading-relaxed">
                            <Link href={path}>{title}</Link>
                        </h1>

                        {keywords.length > 0 && (
                            <div className="md:text-lg md:mt-1">
                                Keywords:
                                <ul className="inline-flex flex-row">
                                    {keywords.map((keyword, i) => (
                                        <li key={keyword} className="ml-1">
                                            <Link
                                                href={`/keywords/${keyword}`}
                                                className="hover:text-sky-700 dark:hover:text-amber-500"
                                            >
                                                {keyword}
                                            </Link>
                                            {i < keywords.length - 1 && ','}
                                        </li>
                                    ))}
                                </ul>
                            </div>
                        )}
                    </header>
                    <div>
                        <div className="global-article" dangerouslySetInnerHTML={{ __html: preface }} />
                        <Link
                            href={path + '#more'}
                            className="block w-48 mt-6 mx-auto border border-zinc-500 dark:border-zinc-600 px-4 py-2 text-center dark:text-white"
                        >
                            続きを読む
                        </Link>
                    </div>
                </article>
            ))}
            {accessory}
        </section>
    );
};

export { PostCollection };
