import Image from 'next/image';
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
        preview: string | null;
        preface: string;
        title: string;
    };
}

const PostCollection: React.FC<PostCollection.Props> = ({ accessory, posts }, i) => {
    return (
        <section className="space-y-6 md:space-y-8">
            {posts.map(({ date: [year, month, day], slug, preface, path, preview, title }) => (
                <article
                    key={slug}
                    className="sm:rounded-xl shadow bg-zinc-50 dark:bg-zinc-900 dark:border-y sm:dark:border dark:border-zinc-700"
                >
                    <header
                        className={
                            `w-full h-48 mb-2 lg:mb-4 relative flex flex-col items-center justify-center text-white text-center bg-zinc-900 dark:bg-zinc-800 sm:rounded-t-xl ` +
                            (preview === null ? '' : 'lg:h-80')
                        }
                    >
                        {preview !== null && (
                            <Image
                                className="brightness-25 sm:rounded-t-xl"
                                src={preview}
                                layout="fill"
                                objectFit="cover"
                                priority={i < 2}
                            />
                        )}
                        <h1 className="text-xl lg:text-3xl font-medium leading-relaxed w-5/6 text-center z-10">
                            <Link href={path}>
                                <a>{title}</a>
                            </Link>
                        </h1>
                        <time className="z-10 mt-2 lg:mt-4 text-base lg:text-xl">
                            {year}-{month}-{day}
                        </time>
                    </header>
                    <div className="px-2 py-4 sm:p-3 md:p-4">
                        <div className="global-article" dangerouslySetInnerHTML={{ __html: preface }} />
                        <Link href={path + '#more'}>
                            <a className="block w-48 mt-4 md:mt-6 mx-auto border border-zinc-500 dark:border-zinc-600 px-4 py-2 text-center dark:text-white">
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
