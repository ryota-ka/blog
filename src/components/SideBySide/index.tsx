declare namespace SideBySide {
    type Props = {
        children: [React.ReactNode, React.ReactNode];
        className?: string;
    };
}

const SideBySide: React.FC<SideBySide.Props> = ({ children, className = '' }: SideBySide.Props) => {
    return (
        <div className={`flex justify-between flex-wrap ${className}`}>
            <div className="w-full lg:w-3/4 max-w-screen-lg">{children[0]}</div>
            <aside className="hidden grow shrink-0 lg:block w-1/4 pl-4 max-w-sm space-y-10">{children[1]}</aside>
        </div>
    );
};

export { SideBySide };
