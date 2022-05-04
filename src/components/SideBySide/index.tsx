declare namespace SideBySide {
    type Props = {
        children: [React.ReactNode, React.ReactNode];
    };
}

const SideBySide: React.FC<SideBySide.Props> = ({ children }: SideBySide.Props) => {
    return (
        <div className="flex justify-center flex-wrap sm:px-2 md:px-4 pt-4">
            <div className="w-full lg:w-3/4 max-w-screen-lg">{children[0]}</div>
            <aside className="hidden shrink-0 lg:block w-1/4 pl-4 max-w-sm">{children[1]}</aside>
        </div>
    );
};

export { SideBySide };
