export const define =
    (name: string) =>
    (constructor: CustomElementConstructor): void =>
        void Promise.race([
            customElements.whenDefined(name),
            new Promise<void>((resolve) => {
                customElements.define(name, constructor);
                resolve();
            }),
        ]);
