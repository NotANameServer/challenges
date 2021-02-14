#!/usr/bin/python3

from collections import UserList


class Heap(UserList):
    def __init__(self, array):
        super().__init__(array)
        for i in range(len(self)):
            self.sift_down(len(self) - i - 1, len(self))

    def swap(self, i, j):
        self[i], self[j] = self[j], self[i]

    def sift_up(self, node, size):
        """
        Compare self, neighbore and parent to find the biggest node,
        if the biggest is one of the children, that child is swap
        with its parent and then this function is call recurcively
        to sort the swapped node against its neighore and parent.
        """

        if not node:
            return

        parent = (node - 1) // 2
        if self[node] > self[parent]:
            self.swap(parent, node)
            self.sift_up(parent, size)

    def sift_down(self, parent, size):
        """
        Compare self and children to find the smallest node,
        if the smallest is one of the children, that child is swap
        with its parent and then this function is call recurcively
        to sort the swapped node against its children.
        """
        lchild = parent * 2 + 1
        rchild = lchild + 1
        if lchild >= size:
            # no children
            pass
        elif rchild >= size:
            # just one child, comp self and child
            if self[lchild] > self[parent]:
                self.swap(parent, lchild)

        else:
            # two children, comp self and children
            bigchild = max(lchild, rchild, key=self.__getitem__)
            if self[bigchild] > self[parent]:
                self.swap(parent, bigchild)
                self.sift_down(bigchild, size)

    def sort(self):
        for i in range(len(self) - 1, -1, -1):
            self.swap(0, i)
            self.sift_down(0, i)

    def __repr__(self):
        return "<Heap [{}]>".format(", ".join(self.data))
